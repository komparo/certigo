certigo_tmp_file <- function() {
  if(!fs::dir_exists(path_workflow(".certigo/tmp"))) {
    fs::dir_create(path_workflow(".certigo/tmp"))
  }
  tempfile(tmpdir = ".certigo/tmp")
}

#' Executors
#'
#' @param ... Other parameters
#'
#' @rdname executor


Executor <- R6Class(
  "Executor",
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

# wraps the /usr/bin/time command to get resources
wrap_command_resources <- function(command, args, resources_file) {
  time_format <- '{"exit_code" : %x, "time_user_seconds" : %U, "time_system_seconds" : %S, "time_wall_clock_seconds" : %e, "rss_max_kbytes" : %M, "rss_avg_kbytes" : %t, "page_faults_major" : %F, "page_faults_minor" : %R, "io_inputs" : %I, "io_outputs" : %O, "context_switches_voluntary" : %w, "context_switches_involuntary" : %c, "cpu_percentage" : "%P", "signals_received" : %k}'
  args <- c("-o", resources_file, "-f", time_format, command, args)
  command <- "/usr/bin/time"

  list(command = command, args = args)
}


ProcessExecutor <- R6Class(
  "ProcessExecutor",
  inherit = Executor,
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



LocalExecutor <- R6Class(
  "LocalExecutor",
  inherit = ProcessExecutor,
  list(
    id = "local"
  ),
  active = list(
    digest = function(...) "local",
    exists = function(...) TRUE
  )
)

#' @rdname executor
#' @export
local_executor <- LocalExecutor$new



DockerExecutor <- R6Class(
  "DockerExecutor",
  inherit = ProcessExecutor,
  public = list(
    container = NULL,
    user_id = "1000",
    tag = NULL,
    initialize = function(container = "rocker/tidyverse") {
      self$container <- container
      self$string <- container
      self$id <- paste0("docker: ", container)
      self$user_id <- system("id -u", intern = TRUE)
    },
    start = function(command, args, resources_file = FALSE) {
      # check whether resources are requested
      # for docker, this requires that /usr/bin/time is installed (https://packages.debian.org/jessie/time)
      # this is the case for ubuntu, but not for debian
      if (!is.null(resources_file) && is.character(resources_file)) {
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
  active = list(
    digest = function(...) "docker",
    exists = function(...) TRUE
  )
)

#' @rdname executor
#' @export
docker_executor <- DockerExecutor$new
