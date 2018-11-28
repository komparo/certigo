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
    encapsulate = function() stop(),
    string = "",
    id = "environment"
  )
)

LocalEnvironment <- R6Class(
  "LocalEnvironment",
  inherit = Environment,
  public = list(
    process = NULL,
    initialize = function() {},
    encapsulate = function(command, args, resources_file = NULL) {
      if (command == "R") {command <- paste0(Sys.getenv("R_HOME"), "/bin/R")} # fix for R CMD check which apparently does not allow running R in processx without running it in R home

      # resources
      if (!is.null(resources_file)) {
        # extra checks to make sure the correct resource_file is given:
        # - character
        # - descendant of the current directory, becuase this will be mounted
        if (is.character(resources_file) && fs::path_has_parent(resources_file, ".")) {
          wrapped <- wrap_command_resources(command, args, resources_file)
          command <- wrapped$command
          args <- wrapped$args
        } else {
          stop("Invalid resources file")
        }
      }

      lst(
        command,
        args
      )
    },
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
  inherit = LocalEnvironment,
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
    encapsulate = function(command, args, resources_file = FALSE, encapsulate_container = TRUE) {
      # check whether resources are requested
      # for docker, this requires that /usr/bin/time is installed (https://packages.debian.org/jessie/time)
      # this is usually not the case (e.g. ubuntu, debian, ...)
      if (!is.null(resources_file) && is.character(resources_file)) {
        if (!is.character(resources_file)) stop("Invalid resources file")
        wrapped <- wrap_command_resources(command, args, resources_file)
        command <- wrapped$command
        args <- wrapped$args
      }

      if (encapsulate_container) {
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
      }

      lst(
        command,
        args
      )
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
