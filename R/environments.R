#' Environments
#'
#' @param ... Other parameters
#'
#' @rdname environment
Environment <- R6Class(
  "Environment",
  inherit = Object,
  list(
    initialize = function() stop(),
    start = function() stop(),
    wait = function() stop(),
    start_and_wait = function() {
      self$start()
      self$wait()
    },
    stop = function() stop(),
    error = NULL,
    output = NULL,
    string = "",
    time = NULL
  ),
  private = list(
    resources_file = NULL
  ),
  active = list(
    status = function() stop()
  )
)

ProcessEnvironment <- R6Class(
  "ProcessEnvironment",
  inherit = Environment,
  public = list(
    process = NULL,
    initialize = function() {},
    start = function(command, args, resources_file = NULL) {
      if (command == "R") {command <- paste0(Sys.getenv("R_HOME"), "/bin/R")} # fix for R CMD check which apparently does not allow running R in processx without running it in R home

      # resources
      if (!is.null(resources_file)) {
        # extra checks to make sure the correct resource_file is given:
        # - character
        # - descendant of the current directory, becuase this will be mounted
        if (is.character(resources_file) && fs::path_has_parent(resources_file, ".")) {
          private$resources_file <- resources_file
          wrapped <- wrap_command_resources(command, args, private$resources_file)
          command <- wrapped$command
          args <- wrapped$args
        } else {
          stop("Invalid resources file")
        }
      }

      # process
      self$process <- processx::process$new(
        command,
        args,
        stdout = "|",
        stderr = "|",
        supervise = TRUE,
        cleanup = TRUE,
        cleanup_tree = TRUE,
        wd = path_workflow()
      )
    },
    wait = function() {
      if(!is.null(self$process)) {
        self$process$wait()
        self$stop()

      } else {
        # process was not started, just return nothing
      }
      invisible()
    },
    stop = function() {
      # read in the output and error
      self$output <- self$process$read_all_output_lines()
      self$error <- self$process$read_all_error_lines()

      self$process$kill_tree()
    },
    reset = function() {
      self$process <- NULL
    }
  ),
  active = list(
    status = function() {
      if(is.null(self$process)) {
        "waiting"
      } else if (self$process$is_alive()) {
        "running"
      } else if(self$process$get_exit_status() > 0) {
        "errored"
      } else {
        "success"
      }
    }
  )
)



LocalEnvironment <- R6Class(
  "LocalEnvironment",
  inherit = ProcessEnvironment,
  list(
    id = "local"
  ),
  active = list(
    digest = function(...) "local",
    exists = function(...) TRUE
  )
)

#' @rdname environment
#' @export
local_environment <- LocalEnvironment$new



DockerEnvironment <- R6Class(
  "DockerEnvironment",
  inherit = ProcessEnvironment,
  public = list(
    container = NULL,
    user_id = "1000",
    tag = NULL,
    initialize = function(container = "rocker/tidyverse") {
      self$container <- container
      self$string <- container
      self$id <- paste0("docker: ", container)
      self$user_id <- system("id -u", intern = TRUE)

      # check if the image exists and its digest
      digest <- processx::run(
        "docker",
        c("inspect", "--type=image", container, paste0("--format='{{ .Id }}'")),
        error_on_status = FALSE
      )$stdout %>%
      stringr::str_replace_all("['\\n]", "")

      private$exists_cached <- digest != ""
      private$digest_cached <- digest
    },
    start = function(command, args, resources_file = FALSE) {
      # check whether resources are requested
      # for docker, this requires that /usr/bin/time is installed (https://packages.debian.org/jessie/time)
      # this is usually not the case (e.g. ubuntu, debian, ...)
      if (!is.null(resources_file) && is.character(resources_file)) {
        if (!is.character(resources_file)) stop("Invalid resources file")
        private$resources_file <- resources_file
        wrapped <- wrap_command_resources(command, args, private$resources_file)
        command <- wrapped$command
        args <- wrapped$args
      }

      # create a random name for the container
      self$tag <- tempfile(tmpdir = "") %>% str_sub(2)

      args <- c(
        "run",
        "-v", glue::glue("{fs::path_abs(path_workflow())}:/data"),
        "-w", "/data",
        "--rm",
        "-u", self$user_id,
        "--name", self$tag,
        self$container,
        command,
        args
      )
      command <- "docker"
      super$start(command, args, resources = NULL)
    }
  ),
  private = list(
    digest_cached = NULL,
    exists_cached = NULL
  ),
  active = list(
    digest = function(...) private$digest_cached,
    exists = function(...) private$exists_cached
  )
)

#' @rdname environment
#' @export
docker_environment <- DockerEnvironment$new