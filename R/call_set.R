CallSet <- R6::R6Class(
  "CallSet",
  public = list(
    calls = list(),
    id = NULL,
    inputs = NULL,
    outputs = NULL,
    design = NULL,
    initialize = function(id, call_class, inputs, outputs, design = NULL) {
      self$id <- id

      # check inputs and outputs tibbles
      inputs <- process_objects(inputs)
      outputs <- process_objects(outputs)

      testthat::expect_equal(nrow(inputs), nrow(outputs))

      # create dummy design if not given
      if (is.null(design)) {
        design <- tibble(id = paste0(id, "_", seq_len(nrow(inputs))))
      }
      design$id <- paste0(id, "_", seq_len(nrow(inputs)))

      # set inputs and outputs of this call set
      self$inputs <- inputs
      self$outputs <- outputs
      self$design <- design

      # create calls
      self$calls <- map(seq_len(nrow(inputs)), function(call_ix) {
        inputs_row <- dynutils::extract_row_to_list(inputs, call_ix)
        outputs_row <- dynutils::extract_row_to_list(outputs, call_ix)
        design_row <- dynutils::extract_row_to_list(design, call_ix)

        call_class$new(design_row$id, inputs_row, outputs_row, design_row)
      })
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
    },
    debug = function() {
      self$calls[[1]]$debug()
    }
  )
)

calls_factory <- function(class) {
  function(id, inputs, outputs, design = NULL) {
    CallSet$new(id, class, inputs, outputs, design = design)
  }
}

process_objects <- function(x) {
  if (is.data.frame(x)) {
    x
  } else if (is.list(x)) {
    testthat::expect_true(length(names(x)) == length(x))
    x %>%
      map(function(y) {if(is.atomic(y)) {y} else {list(y)}}) %>%
      as_tibble()
  } else {
    stop("Invalid inputs/outputs object")
  }
}









CallCollection <- R6::R6Class(
  "CallCollection",
  public = list(
    calls = list(),
    inputs = NULL,
    outputs = NULL,
    design = NULL,
    initialize = function(id, ...) {
      # create calls
      self$calls <- list(...) %>% map("calls") %>% flatten()

      # adapt ids
      walk(self$calls, function(call) {
        call$id <- paste0(id, "/", call$id)
        call$design$id <- call$id
      })

      # check call ids
      call_ids <- map(self$calls, "id")
      if (any(duplicated(call_ids))) {
        stop("Duplicated call ids: ", unique(call_ids[duplicated(call_ids)]) %>% glue::glue_collapse(", "))
      }

      # merge inputs, outputs and design
      self$inputs <- self$calls %>% map("inputs") %>% dynutils::list_as_tibble()
      self$outputs <- self$calls %>% map("outputs") %>% dynutils::list_as_tibble()
      self$design <- self$calls %>% map("design") %>% dynutils::list_as_tibble()
      self$design$id <- self$calls %>% map_chr("id")
    }
  )
)

#' Call collection
#' @param ... Call sets
#' @export
call_collection <- CallCollection$new
