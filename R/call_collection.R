CallCollection <- R6::R6Class(
  "CallCollection",
  public = list(
    calls = list(),
    id = NULL,
    design = tibble(),
    initialize = function(id, calls, design) {
      self$id <- id
      self$calls <- calls
      self$design <- design
    },
    start = function() {
      called <- map(self$calls, "start") %>% invoke_map()

      invisible()
    },
    wait = function() {
      waited <- map(self$calls, "wait") %>% invoke_map()
    },
    start_and_wait = function() {
      self$start()
      self$wait()
    }
  )
)

# Create either a call collection or one call depending on whether a design was given
# This function will return another function which can be used to construct a call or set of calls using a Call subclass
calls_factory <- function(class) {
  function(id, ..., design = NULL, params = list()) {
    testthat::expect_true(is.null(design) || is.data.frame(design))

    if(is.null(design)) {
      # if no design is given, directly create a call
      class$new(id, ...)
    } else {
      # add an id column, even if it is already present
      design$id <- label_design(id, design)
      testthat::expect_true(!any(duplicated(design$id)))

      argument_expressions <- rlang::exprs(...)

      # create individual calls by evaluating the ... inside the environment of a row in the design tibble
      calls <- pmap(design, function(...) {
        design_row <- list(...)

        # evaluate the arguments for this design row
        arguments <- purrr::map(argument_expressions, rlang::eval_tidy, data = list_modify(params, !!!design_row))

        # create an extra input which includes the design
        design_parameters <- parameters(design_row)
        if ("inputs" %in% arguments) {
          arguments$inputs <- c(arguments$inputs, list(design = design_parameters))
        } else {
          arguments$inputs <- list(design = design_parameters)
        }

        # create a unique id based on the calls id and the parameters
        arguments$id <- design_row$id

        # create calls
        invoke(class$new, arguments)
      })

      CallCollection$new(id, calls, design)
    }
  }
}

label_design <- function(id, design) {
  # if design has an id column, this is simple
  if ("id" %in% names(design)) {
    design[["id"]]
  } else {
    paste0(id, "_", seq_len(nrow(design)))
  }
}
