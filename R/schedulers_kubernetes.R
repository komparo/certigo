KubernetesScheduler <- R6::R6Class(
  "KubernetesScheduler",
  public = list(
    initialize = function() {

    },
    start = function(command, args = character(), environment, resources_file = NULL) {
      testthat::expect_is(environment, "DockerEnvironment")

      # encapsulate, but without the container call itself which will be handled by kubernetes
      encapsulated <- environment$encapsulate(
        command,
        args,
        resources_file = resources_file,
        encapsulate_container = FALSE
      )

      # create config
      config <- generate_kubernetes_config(
        container = environment$container,
        command = encapsulated$command,
        args = encapsulated$args,
        host_path = fs::path_abs(path_workflow())
      )

      # start pod
      system(glue::glue("kubectl create -f {config$config_path}"))

      # get pod_id
      config$pod_id <- system(glue::glue("kubectl get pods --selector=job-name={config$kubernetes_job_name} --output=jsonpath={{.items..metadata.name}}"), intern = TRUE)

      if (is.null(config$pod_id) || nchar(config$pod_id) == 0) {
        stop("Could not find id of pod for job ", config$kubernetes_job_name)
      }

      # save
      job_id <- config$kubernetes_job_name

      private$configs[[job_id]] <- config

      job_id
    },
    status = function(job_id) {
      if (is.null(job_id)) {
        "pending"
      } else {
        pod_id <- private$configs[[job_id]]$pod_id
        get_kubernetes_status(pod_id)
      }
    },
    wait = function(job_id) {
      pod_id <- private$configs[[job_id]]$pod_id
      status <- get_kubernetes_status(pod_id)
      while(status %in% c("pending", "running")) {
        status <- get_kubernetes_status(pod_id)
        Sys.sleep(1)
      }
      self$finish(job_id)
    },
    finish = function(job_id) {
      config <- private$configs[[job_id]]

      # read output
      output <- list(
        output = read_kubernetes_log(config$pod_id),
        error = read_kubernetes_failure(config$pod_id),
        status = self$status(job_id)
      )

      # delete job
      delete_kubernetes_job(job_id)

      # garbage collect config
      private$configs[[job_id]] <- NULL

      output
    }
  ),
  private = list(
    configs = list()
  )
)

#' Kubernetes scheduler
#' @export
#' @param ... ...
kubernetes_scheduler <- KubernetesScheduler$new




generate_kubernetes_config <- function(container, command, args, host_path) {
  config_path <- tempfile()
  kubernetes_job_name <- kubernetes_container_name <- paste0("certigo", sample(1:10000, 1))

  host_path <- fs::path_abs(host_path)
  # home directory gets automatically mounted inside minikube
  # so the host_path has to be a child of that
  if (!fs::path_has_parent(host_path, fs::path_home())) {
    stop("For kubernetes, workflow directory has to be child of home directory")
  }

  host_path <- host_path %>% str_replace_all("/home/", "/hosthome/")

  config <- list(
    apiVersion = "batch/v1",
    kind = "Job",
    metadata = list(
      name = kubernetes_job_name
    ),
    spec = list(
      backoffLimit = 1,
      template = list(
        spec = list(
          containers = list(
            list(
              name = kubernetes_container_name,
              image = container,
              command = as.list(c(command, args)),
              workingDir = "/data",
              volumeMounts = list(
                list(
                  mountPath = "/data",
                  name = "certigo-data"
                )
              )
            )
          ),
          restartPolicy = "Never",
          volumes = list(
            list(
              name = "certigo-data",
              hostPath = list(
                path = host_path,
                type = "Directory"
              )
            )
          )
        )
      ),
      backoffLimit = 1
    )
  )

  config %>% yaml::write_yaml(config_path)

  lst(
    kubernetes_job_name,
    kubernetes_container_name,
    config_path
  )
}


get_kubernetes_status <- function(pod_id) {
  get_pod <- system(glue::glue("kubectl get pods {pod_id} -o=json"), intern = TRUE) %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)
  phase <- get_pod$status$phase
  container_status <- get_pod$status$containerStatuses[[1]]$state[[1]]$reason %||% "ok"

  # test if the image cannot be pulled
  if (
    phase == "Pending" &&
    container_status %in% c("ErrImagePull", "ImagePullBackOff")
  ) {
    status <- "failed"
  } else {
    if (phase == "Unknown") {
      stop(glue::glue("Phase of pod '{names(status)[status == 'unknown']}' is unknown, something went wrong"))
    }
    status <- tolower(phase)

    # if a job is pending, it is actually running acording to our standards
    status[status == "pending"] <- "running"
  }

  status
}


delete_kubernetes_job <- function(kubernetes_job_name) {
  # will also delete any associated pods
  system(glue::glue("kubectl delete job {kubernetes_job_name}"))
}


read_kubernetes_log <- function(pod_id) {
  system(glue::glue("kubectl logs {pod_id}"), intern = TRUE)
}


read_kubernetes_failure <- function(pod_id) {
  system(
    paste0(
      "kubectl get pod ",
      pod_id,
      " -o go-template='{{range .status.containerStatuses}}{{.state.terminated.message}}{{end}}'"
    ),
    intern = TRUE
  )
}
