#' A call
#'
#' @param id The name of the call
#' @param inputs The inputs
#' @param outputs The outputs
#' @param design The design
#' @rdname call
Call <- R6Class(
  "Call",
  public = list(
    id = NULL,
    inputs = NULL,
    outputs = NULL,
    design = NULL,
    executor = NULL,
    command = NULL,
    args = NULL,
    cached = FALSE,
    initialize = function(id, inputs, outputs, design = NULL) {
      self$id <- id

      # check inputs and outputs ----------------------
      # test whether all inputs and ouputs are named
      testthat::expect_true(length(unique(names(inputs))) == length(inputs), "All inputs should be named")
      testthat::expect_true(length(unique(names(outputs))) == length(outputs), "All outputs should be named")

      # test whether all inputs and outputs are objects
      testthat::expect_true(all(map_lgl(inputs, ~"Object" %in% class(.))), "All inputs should be an Object")
      testthat::expect_true(all(map_lgl(outputs, ~"Object" %in% class(.))), "All outputs should be an Object")

      # test whether all rows match
      testthat::expect_equal(nrow(inputs), nrow(outputs))
      if (!is.null(design)) {
        testthat::expect_equal(nrow(inputs), nrow(design))
      }

      # add local executor if not present in inputs
      if (!"executor" %in% names(inputs)) {
        inputs$executor <- local_executor()
      }
      self$executor <- inputs$executor$clone()

      # add inputs & outputs to self
      self$inputs <- inputs
      self$outputs <- outputs
      self$design <- design
    },
    start = function() {
      # make sure all inputs are present
      existing_input <- map_lgl(self$inputs, function(input) {
        if (TRUE && !input$exists) {
          cat_line(col_split(crayon_error("\U274C Input does not exist: ", input$id), self$id))
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
      # if an output is not present, its call digest will be NA, which will always trigger a rerun
      if(all(!is.na(output_call_digests)) && all(output_call_digests == call_digest)) {
        # cached
        cat_line(col_split(crayon_ok("\U23F0 Cached"), self$id))
        self$cached <- TRUE
      } else {
        # start the executor
        self$executor$start(self$command, self$args)
        cat_line(col_split(crayon_info("\U25BA Started"), self$id))
        self$cached <- FALSE
      }
    },
    start_and_wait = function() {
      self$start()
      self$wait()
    },
    wait = function() {
      if (!self$cached) {
        self$executor$wait()

        # Check whether the process successfully finished
        if (self$executor$status %in% c("success")) {
          # do nothing
        } else if (self$executor$status %in% c("errored")) {
          cat_line(col_split(crayon_error("\U274C Errored"), self$id))
          map(self$outputs, "delete") %>% invoke_map()
          cat_line(self$executor$error %>% tail(10))
          stop(crayon_error("Process errored"), call. = FALSE)
        } else {
          stop("Process neither did not success nor error, was it started?")
        }

        # check the output
        output_validations <- map_lgl(self$outputs, function(output) {
          validation <- output$validate(self$design)
          if (is.character(validation)) {
            cat_line(col_split(crayon_warning("\U274C Validation"), self$id))
            cat_line(crayon_warning("File: ", crayon::italic(output$id)))
            cat_line(crayon_warning("Problem: ", crayon::bold(validation)))
            FALSE
          } else {
            TRUE
          }
        })

        # if output is not valid:
        # -> Delete all outputs
        # -> Stop execution
        if (!all(output_validations)) {
          cat_line(col_split(crayon_error("\U274C Output"), self$id))
          map(self$outputs, "delete") %>% invoke_map()
          stop(crayon_error("Some output not valid"), call. = FALSE)
        }

        cat_line(col_split(crayon_ok("\U2714 Finished"), self$id))

        # write all output histories, which includes the digest of the call
        walk(self$outputs, function(output) {
          output$write_history(call_digest = self$digest)
        })

        # cleanup the executor
        self$executor$stop()
      }
    },
    reset = function() {
      self$executor$reset()
      self$cached <- FALSE
    }
  ),
  active = list(
    label = function(...) fontawesome_map["play"],
    digest = function() {
      stop("Digest not implemented for this call")
    },
    status = function(...) {
      if (self$cached) {
        "cached"
      } else {
        self$executor$status
      }
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
    command = "R",
    initialize = function(
      id,
      inputs = list(),
      outputs = list(),
      design = NULL,
      source_command = glue::glue("source('{inputs$script$string}')")
    ) {
      testthat::expect_true("script" %in% names(inputs))

      super$initialize(id, inputs, outputs, design)

      # get input and output strings
      # first filter the script and executor out
      input_strings <- self$inputs[-which(names(self$inputs) %in% c("script", "executor"))] %>% map("string")
      output_strings <- self$outputs %>% map("string")

      fs::dir_create(".certigo/object_sets", recursive = TRUE)
      input_path <- tempfile(tmpdir = ".certigo/object_sets")
      jsonlite::write_json(input_strings, input_path)
      output_path <- tempfile(tmpdir = ".certigo/object_sets")
      jsonlite::write_json(output_strings, output_path)

      self$args <- c(
        "-e",
        glue::glue("inputs <- jsonlite::read_json('{input_path}', simplifyVector = TRUE);outputs <- jsonlite::read_json('{output_path}', simplifyVector = TRUE);pdf(NULL);{source_command}")
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



RmdCall <- R6Class(
  "RmdCall",
  inherit = RscriptCall,
  public = list(
    initialize = function(id, inputs, outputs, ...) {
      testthat::expect_true("script" %in% names(inputs), info = "Should at least specify an 'script' as inputs")
      testthat::expect_true("rendered" %in% names(outputs), info = "Should at least specify an 'rendered' as outputs")

      rmd_path <- inputs[["script"]]$string
      output_dir <- fs::path_dir(outputs[["rendered"]]$string)
      output_file <- fs::path_file(outputs[["rendered"]]$string)

      source_command <- glue::glue_collapse(glue::glue(
        "knit_root_dir = fs::path_abs('.');",
        "rmarkdown::render(",
        "'{rmd_path}',",
        "output_dir = '{output_dir}',",
        "output_file = '{output_file}',",
        "knit_root_dir = knit_root_dir",
        ")"
      ))

      super$initialize(
        id,
        inputs,
        outputs,
        ...,
        source_command = source_command
      )
    }
  )
)


#' @export
#' @rdname call
rmd_call <- calls_factory(RmdCall)
