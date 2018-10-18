#' A call
#'
#' @param id The name of the call
#' @param ... Extra arguments to the call, such as inputs, outputs, script, ...
#' @param design A dataframe containing the information for each call in a separate row
#' @param params A list of extra parameters
#' @rdname call
Call <- R6Class(
  "Call",
  public = list(
    id = NULL,
    inputs = NULL,
    outputs = NULL,
    executor = NULL,
    command = NULL,
    args = NULL,
    initialize = function(id, inputs, outputs, executor = local_executor()) {
      self$id <- id

      testthat::expect_true(all(map_lgl(inputs, ~"Object" %in% class(.))), "All inputs should be an Object")
      testthat::expect_true(all(map_lgl(outputs, ~"Object" %in% class(.))), "All outputs should be an Object")
      testthat::expect_true("Executor" %in% class(executor), "The executor should be an Executor")

      self$executor <- executor
      self$inputs <- inputs
      self$outputs <- outputs
    },
    start = function() {
      # make sure all inputs are present
      existing_input <- map_lgl(self$inputs, function(input) {
        if (TRUE && !input$exists) {
          cat_line(col_split(self$id, crayon_error("\U274C Input does not exist: ", input$id)))
          FALSE
        } else {
          TRUE
        }
      })

      if (any(!existing_input)) {
        map(self$outputs, "delete") %>% invoke_map()
        stop(col_split(self$id, crayon_error("\U274C Input ")), call. = FALSE)
      }

      # check whether all call_digests of the outputs match with the current output digest
      output_call_digests <- map_chr(self$outputs, "call_digest")
      call_digest <- self$digest

      # choose between cached or actual execution
      if(all(!is.na(output_call_digests)) && all(output_call_digests == call_digest)) {
        # cached
        cat_line(col_split(self$id, crayon_ok("\U23F0 Cached")))
      } else {
        # start the executor
        self$executor$start(self$command, self$args)
        cat_line(col_split(self$id, crayon_info("\U25BA Started")))
      }
    },
    start_and_wait = function() {
      self$start()
      self$wait()
    },
    wait = function() {
      if (self$executor$status == "running") {
        self$executor$wait()

        if (self$executor$status %in% c("success")) {
          cat_line(col_split(self$id, crayon_ok("\U2714 Finished")))
        } else if (self$executor$status %in% c("errored")) {
          cat_line(col_split(self$id, crayon_error("\U274C Errored")))
          map(self$outputs, "delete") %>% invoke_map()
          cat_line(self$executor$error %>% tail(5))
        }

        # check whether output is present
        existing_output <- map_lgl(self$outputs, function(output) {
          if (TRUE && !output$exists) {
            cat_line(col_split(self$id, crayon_error("\U274C Output does not exist: ", output$id)))
            FALSE
          } else {
            TRUE
          }
        })

        # if some output is not present, error
        if (any(!existing_output)) {
          cat_line(col_split(self$id, crayon_error("\U274C Output")))
          map(self$outputs, "delete") %>% invoke_map()
          stop("Some output not present but required")
        }

        # write all output histories including the digest of the call
        walk(self$outputs, function(output) {
          output$write_history(call_digest = self$digest)
        })

        # cleanup the executor
        self$executor$stop()
      }
    }
  ),
  active = list(
    label = function(...) fontawesome_map["play"],
    digest = function() {
      stop("Digest not implemented for this call")
    }
  )
)



#   ____________________________________________________________________________
#   R script                                                                ####

deparse_friendly <- function(x) {
  deparse(x, width.cutoff = 500) %>% glue::glue_collapse("")
}


RscriptCall <- R6Class(
  "RscriptCall",
  inherit = Call,
  public = list(
    command = paste0("R"),
    initialize = function(id, script, inputs = list(), outputs = list(), executor = local_executor()) {
      super$initialize(id, c(list(script), inputs), outputs, executor)

      input_strings <- inputs %>% map_chr("string")
      output_strings <- outputs %>% map_chr("string")

      self$args <- c(
        "-e",
        glue::glue("inputs <- {deparse_friendly(input_strings)};outputs <- {deparse_friendly(output_strings)};pdf(NULL);source('{script$string}')")
      )
    },
    debug = function() {
      clipr::write_clip(self$args[[2]])
      message("Inputs and outputs written to clipboard")
    }
  ),
  active = list(
    digest = function() {
      input_digests <- map(self$inputs, "digest")
      output_strings <- map(self$outputs, "string")
      paste0(
        "R ",
        glue::glue_collapse(input_digests, " "),
        " ",
        glue::glue_collapse(output_strings, " ")
      )
    }
  )
)

#' @export
#' @rdname call
rscript_call <- calls_factory(RscriptCall)
