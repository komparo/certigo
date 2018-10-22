CallSet <- R6::R6Class(
  "CallSet",
  public = list(
    calls = list(),
    id = NULL,
    inputs = NULL,
    outputs = NULL,
    initialize = function(id, call_class, inputs, outputs) {
      self$id <- id

      # check inputs and outputs tibbles
      inputs <- process_objects(inputs)
      outputs <- process_objects(outputs)

      testthat::expect_equal(nrow(inputs), nrow(outputs))

      # set inputs and outputs of this call set
      self$inputs <- inputs
      self$outputs <- outputs

      # create calls
      self$calls <- map(seq_len(nrow(inputs)), function(call_ix) {
        inputs_row <- dynutils::extract_row_to_list(inputs, call_ix)
        outputs_row <- dynutils::extract_row_to_list(outputs, call_ix)

        id_row <- paste0(id, "_", call_ix)

        call_class$new(id_row, inputs_row, outputs_row)
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
  function(id, inputs, outputs) {
    CallSet$new(id, class, inputs, outputs)
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
