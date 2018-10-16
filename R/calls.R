#' A call
#'
#' @param id The name of the call
#' @param ... Extra arguments to the call, such as inputs, outputs, script, ...
#' @param design A dataframe containing the information for each call in a separate row
#' @rdname call
Call <- R6Class(
  "Call",
  public = list(
    id = NULL,
    command = NULL,
    inputs = list(),
    outputs = list(),
    args = character(),
    process = NULL,
    initialize = function(id, inputs, outputs) {
      self$id <- id

      testthat::expect_true(all(map_lgl(inputs, ~"Object" %in% class(.))), "All inputs should be an object")
      testthat::expect_true(all(map_lgl(outputs, ~"Object" %in% class(.))), "All outputs should be an object")

      self$inputs <- inputs
      self$outputs <- outputs
    },
    call = function(wait = FALSE) {
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

      if(all(!is.na(output_call_digests)) && all(output_call_digests == call_digest)) {
        cat_line(col_split(self$id, crayon_ok("\U23F0 Cached")))
        self$process <- NULL
      } else {
        cat_line(col_split(self$id, crayon_info("\U25BA Started")))
        self$process <- processx::process$new(
          self$command,
          self$args,
          stdout = "|",
          stderr = "|",
          supervise = TRUE,
          cleanup_tree = TRUE
        )
      }
    },
    run = function() {
      self$call()
      self$wait()
    },
    wait = function() {
      if (!is.null(self$process)) {
        self$process$wait()

        if (self$process$get_exit_status() == 0) {
          cat_line(col_split(self$id, crayon_ok("\U2714 Finished")))
        } else {
          cat_line(col_split(self$id, crayon_error("\U274C Errored")))
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
          stop()
        }

        # write all output histories including the digest of the call
        walk(self$outputs, function(output) {
          output$write_history(call_digest = self$digest)
        })
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
    command = paste0(Sys.getenv("R_HOME"), "/bin/R"),
    initialize = function(id, script, inputs = list(), outputs = list()) {
      super$initialize(id, c(list(script), inputs), outputs)

      input_strings <- inputs %>% map_chr("string")
      output_strings <- outputs %>% map_chr("string")

      self$args <- c(
        "-e",
        glue::glue("inputs <- {deparse_friendly(input_strings)};outputs <- {deparse_friendly(output_strings)};source('{script$string}')")
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
        "Rscript ",
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



#   ____________________________________________________________________________
#   Docker                                                                  ####

DockerCall <- R6Class(
  "DockerCall",
  inherit = Call,
  public = list(
    command = "docker",
    initialize = function(id, container, inputs = list(), outputs = list()) {
      super$initialize(id, c(list(container), inputs), outputs)

      input_strings <- inputs %>% map_chr("string")
      output_strings <- outputs %>% map_chr("string")

      self$args <- c(
        "run",
        "-v", glue::glue("{fs::path_abs('.')}:/data"),
        "-w", "/data",
        container$string,
        input_strings,
        output_strings
      )
    }
  ),
  active = list(
    digest = function() {
      input_digests <- map(self$inputs, "digest")
      output_strings <- map(self$outputs, "string")
      paste0(
        "docker ",
        glue::glue_collapse(input_digests, " "),
        " ",
        glue::glue_collapse(output_strings, " ")
      )
    }
  )
)

#' @export
#' @rdname call
docker_call <- calls_factory(DockerCall)
