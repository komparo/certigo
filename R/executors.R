#' Executors
#'
#' @param ... Other parameters
#'
#' @rdname executor


Executor <- R6Class(
  "Executor",
  list(
    initialise = function() stop(),
    start = function() stop(),
    wait = function() stop(),
    start_and_wait = function() {
      self$start()
      self$wait()
    },
    stop = function() stop(),
    error = NULL,
    output = NULL
  ),
  active = list(
    status = function() stop()
  )
)

ProcessExecutor <- R6Class(
  "ProcessExecutor",
  inherit = Executor,
  public = list(
    process = NULL,
    start = function(command, args) {
      if (command == "R") {command <- paste0(Sys.getenv("R_HOME"), "/bin/R")} # fix for R CMD check which apparently does not allow running R in processx without running it in R home

      self$process <- processx::process$new(
        command,
        args,
        stdout = "|",
        stderr = "|",
        supervise = TRUE,
        cleanup_tree = TRUE
      )
    },
    wait = function() {
      if(!is.null(self$process)) {
        self$process$wait()

        # read in the output and error
        self$output <- self$process$read_all_output_lines()
        self$error <- self$process$read_all_error_lines()
        self$process$kill_tree()
      } else {
        # process was not started, just return nothing
      }
    },
    stop = function() {
      self$process <- NULL
    }
  ),
  active = list(
    status = function() {
      if(is.null(self$process)) {
        "setup"
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
  inherit = ProcessExecutor
)

#' @rdname executor
#' @export
local_executor <- LocalExecutor$new



DockerExecutor <- R6Class(
  "DockerExecutor",
  inherit = ProcessExecutor,
  public = list(
    container = NULL,
    initialize = function(container = "rocker/tidyverse") {
      self$container <- container
    },
    start = function(command, args) {
      args <- c(
        "run",
        "-v", glue::glue("{fs::path_abs('.')}:/data"),
        "-w", "/data",
        self$container,
        command,
        args
      )
      command <- "docker"
      super$start(command, args)
    }
  )
)

#' @rdname executor
#' @export
docker_executor <- DockerExecutor$new
