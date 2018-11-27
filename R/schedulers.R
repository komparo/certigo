Scheduler <- R6::R6Class(
  "Scheduler",
  public = list(
    start = function(command, args, environment) {

    }
  )
)





LocalScheduler <- R6::R6Class(
  "LocalScheduler",
  public = list(
    initialize = function() {

    },
    start = function(command, args, environment) {

    }
  ),
  private = list(

  )
)

#' Local scheduler
#' @export
#' @param ... ...
local_scheduler <- LocalScheduler$new







KubernetesScheduler <- R6::R6Class(
  "KubernetesScheduler",
  public = list(
    initialize = function() {

    },
    start = function(command, args, environment) {

    }
  )
)

#' Kubernetes scheduler
#' @export
#' @param ... ...
kubernetes_scheduler <- KubernetesScheduler$new
