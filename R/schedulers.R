Scheduler <- R6::R6Class(
  "Scheduler",
  public = list(
    start = function(command, args, environment) {

    }
  )
)





LocalScheduler <- R6::R6Class(
  "LocalScheduler",
  inherit = Scheduler,
  public = list(
    initialize = function() {},
    start = function(command, args = character(), environment = local_environment(), resources_file = NULL) {
      encapsulated <- environment$encapsulate(command, args, resources_file = resources_file)

      # process
      process <- processx::process$new(
        encapsulated$command,
        encapsulated$args,
        stdout = "|",
        stderr = "|",
        supervise = TRUE,
        cleanup = TRUE,
        cleanup_tree = TRUE,
        wd = path_workflow()
      )

      job_id <- as.character(length(private$processes) + 1)
      private$processes[[job_id]] <- process

      job_id
    },
    status = function(job_id) {
      if (is.null(job_id)) {
        "pending"
      } else {
        process <- private$processes[[job_id]]
        if (process$is_alive()) {
          "running"
        } else if(process$get_exit_status() > 0) {
          "failed"
        } else {
          "succeeded"
        }
      }
    },
    wait = function(job_id) {
      process <- private$processes[[job_id]]
      process$wait()
      self$finish(job_id)
    },
    finish = function(job_id) {
      process <- private$processes[[job_id]]

      # read in the output and error
      output <- list(
        output = process$read_all_output_lines(),
        error = process$read_all_error_lines(),
        status = self$status(job_id)
      )

      process$kill_tree()

      # garbage collect process
      private$processes[[job_id]] <- NULL

      output
    }
  ),
  private = list(
    processes = list()
  )
)

#' Local scheduler
#'
#' There is always only one local scheduler, also available as an option "certigo_local_scheduler"
#'
#' @export
#' @param ... ...
local_scheduler <- function(...) {
  getOption(
    "certigo_local_scheduler",
    {
      scheduler <- LocalScheduler$new(...)
      options(certigo_local_scheduler = scheduler)
      scheduler
    }
  )
}
