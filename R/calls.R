


Call <- R6Class(
  "Call",
  inherit = Object,
  public = list(
    command = NULL,
    inputs = list(),
    outputs = list(),
    args = character(),
    initialize = function(id, inputs, outputs) {
      self$id <- id
      self$inputs <- inputs
      self$outputs <- outputs
    },
    digest = function(input_digests = NULL) {
      stop("Digest not implemented for this call")
    },
    input_status = function(waiting_input_ids = character()) {
      input_digests <- self$inputs %>% map("digest") %>% invoke_map_chr()
      input_ids <- self$inputs %>% map_chr("id")
      case_when(
        all(!is.na(input_digests)) && all(!input_ids %in% waiting_input_ids) ~ "ready",
        TRUE ~ "waiting"
      )
    },
    output_status = function() {
      output_digests <- self$outputs %>% map("digest") %>% invoke_map_chr()
      case_when(
        all(!is.na(output_digests)) ~ "available",
        TRUE ~ "unavailable"
      )
    },
    call_status = function(runs_exited = tibble(digest = character())) {
      digest <- self$digest()

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
    label = function(...) fontawesome_map["play"]
  )
)



RscriptCall <- R6Class(
  "RscriptCall",
  inherit = Call,
  public = list(
    command = "Rscript",
    initialize = function(id, script, inputs = list(), outputs = list()) {
      inputs <- c(list(script), inputs)
      super$initialize(id, inputs, outputs)

      self$args <- c(inputs %>% map_chr("string"), outputs %>% map_chr("string"))
    },
    digest = function() {
      input_digests <- map(self$inputs, "digest") %>% invoke_map_chr()
      output_digests <- map(self$outputs, "digest") %>% invoke_map_chr()
      paste0(
        "Rscript ",
        glue::glue_collapse(input_digests, " "),
        glue::glue_collapse(output_digests, " ")
      )
    }
  )
)

rscript_call <- RscriptCall$new

