Call <- R6Class(
  "Call",
  inherit = Object,
  public = list(
    command = NULL,
    input_ids = list(),
    output_ids = list(),
    inputs = list(),
    outputs = list(),
    objects = list(),
    args = character(),
    initialize = function(id, inputs, outputs) {
      self$id <- id
      self$input_ids <- inputs %>% map_chr("id")
      self$output_ids <- outputs %>% map_chr("id")
      self$objects <- c(inputs, outputs)
    },
    input_status = function(waiting_input_ids = character()) {
      input_digests <- self$inputs %>% map_chr("digest")
      case_when(
        all(!is.na(input_digests)) && all(!self$input_ids %in% waiting_input_ids) ~ "ready",
        TRUE ~ "waiting"
      )
    },
    output_status = function() {
      output_digests <- self$outputs %>% map_chr("digest")
      case_when(
        all(!is.na(output_digests)) ~ "present",
        TRUE ~ "not_present"
      )
    },
    call_status = function(runs_exited) {
      digest <- self$digest

      if (digest %in% runs_exited$digest) {
        "finished"
      } else {
        "unfinished"
      }
    },
    run = function() {
      process <- processx::process$new(self$command, self$args, stdout = "|", stderr = "|", supervise = TRUE, cleanup_tree = TRUE)
    }
  ),
  active = list(
    label = function(...) fontawesome_map["play"],
    digest = function(objects, input_digests = NULL) {
      stop("Digest not implemented for this call")
    }
  )
)



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


      # self$args <- c(, outputs %>% map_chr("string"))
    }
  ),
  active = list(
    digest = function() {
      input_digests <- map(self$inputs, "digest")
      output_digests <- map(self$outputs, "digest")
      paste0(
        "Rscript ",
        glue::glue_collapse(input_digests, " "),
        glue::glue_collapse(output_digests, " ")
      )
    }
  )
)

rscript_call <- RscriptCall$new






deparse_friendly <- function(x) {
  deparse(x, width.cutoff = 500) %>% glue::glue_collapse("")
}
