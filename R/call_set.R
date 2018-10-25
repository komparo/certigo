CallSet <- R6::R6Class(
  "CallSet",
  public = list(
    calls = list(),
    design = NULL,
    id = NULL,
    input_ids = NULL,
    output_ids = NULL,
    initialize = function(id, call_class, design, input_ids, output_ids) {
      self$id <- id

      design <- process_objects(design)
      design$id <- paste0(id, "_", seq_len(nrow(design)))

      testthat::expect_true(all(input_ids %in% names(design)))
      testthat::expect_true(all(output_ids %in% names(design)))

      self$design <- design
      self$input_ids <- input_ids
      self$output_ids <- output_ids

      # create calls
      self$calls <- pmap(design, function(...) {
        design_row <- list(...)

        call_class$new(design_row$id, design = design_row, inputs = design_row[self$input_ids], outputs = design_row[self$output_ids])
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
  ),
  active = list(
    inputs = function(...) self$design %>% select(!!self$input_ids),
    outputs = function(...) self$design %>% select(!!self$output_ids)
  )
)

calls_factory <- function(class) {
  function(id, inputs, outputs, design = NULL) {
    # input_ids <- rlang::enquo(inputs)
    # output_ids <- rlang::enquo(outputs)
    CallSet$new(id, class, design, input_ids = inputs, output_ids = outputs)
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
      # create common calls and common design
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
      self$design <- self$calls %>% map("design") %>% dynutils::list_as_tibble()
      self$design$id <- self$calls %>% map_chr("id")
    }
  )
)

#' Call collection
#' @param ... Call sets
#' @export
call_collection <- CallCollection$new
