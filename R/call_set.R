# concatenate the ids with forward slashes in between
concatenate_ids <- function(a, b) {
  if (is.null(a) || a == "") {
    return(b)
  }
  if (is.null(b) || b == "") {
    return(a)
  }
  if (is.numeric(a)) {
    a <- as.character(a)
  }
  if (is.numeric(b)) {
    b <- as.character(b)
  }
  if (!endsWith(a, "/") && !startsWith(b, "/")) {
    a <- paste0(a, "/")
  }
  paste0(a, b)
}



CallSet <- R6::R6Class(
  "CallSet",
  public = list(
    design = NULL,
    id = NULL,
    inputs = NULL,
    outputs = NULL,
    initialize = function(id, call_class, design, input_exprs, output_exprs) {
      self$id <- id

      design <- process_objects(design)

      # add id to existing id, or create new unique id
      if ("id" %in% names(design)) {
        design$id <- concatenate_ids(id, design$id)
      } else {
        design$id <- concatenate_ids(id, seq_len(nrow(design)))
      }

      self$design <- design
      self$inputs <- design %>% transmute(!!!input_exprs)
      self$outputs <- design %>% transmute(!!!output_exprs)

      # create calls
      self$design$calls <- map(seq_len(nrow(self$design)), function(call_ix) {
        design <- self$design %>% dynutils::extract_row_to_list(call_ix)
        inputs <- self$inputs %>% dynutils::extract_row_to_list(call_ix)
        outputs <- self$outputs %>% dynutils::extract_row_to_list(call_ix)

        call_class$new(
          design$id,
          design = design,
          inputs = inputs,
          outputs = outputs
        )
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
    calls = function(...) self$design$calls
  )
)

calls_factory <- function(class) {
  function(id = "", design, inputs, outputs) {
    CallSet$new(id, class, design, input_exprs = inputs, output_exprs = outputs)
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
    design = NULL,
    initialize = function(id, ...) {
      self$design <- map(list(...), "design") %>% bind_rows()

      # adapt ids
      if (id != "") {
        walk(self$design$calls, function(call) {
          call$id <- concatenate_ids(id, call$id)
          call$design$id <- call$id
        })

        self$design$id <- map_chr(self$design$calls, "id")
      }

      # check for duplicated ids
      if (any(duplicated(self$design$id))) {
        stop("Duplicated call ids: ", unique(self$design$id[duplicated(self$design$id)]) %>% glue::glue_collapse(", "))
      }
    }
  ),
  active = list(
    calls = function(...) self$design$calls
  )
)

#' Call collection
#' @param ... Call sets
#' @export
call_collection <- CallCollection$new


#' Load a call from an R file, given by a "get_call" function in that R file
#'
#' @param call_path The location of the R file which, when sourced, contains a "get_call" function
#' @param derived_file_directory Optional, the location in which derived files should be stored
#' @param id The id of the call
#' @param ... Other parameters given to the get_call function
#'
#' @export
load_call <- function(
  call_path,
  derived_file_directory = "./",
  id = call_path,
  ...
) {
  call_environment <- new.env()

  source(path_workflow(call_path), local = call_environment)
  call_generator <- get("get_call", call_environment)

  # load call
  call <- withr::with_options(
    list(
      workflow_directory = fs::path_dir(call_path),
      derived_file_directory = derived_file_directory
    ),
    call_generator(...)
  )

  # adapt ids
  walk(call$design$calls, function(call) {
    call$id <- concatenate_ids(id, call$id)
    call$design$id <- call$id
  })
  call$design$id <- call$design$calls %>% map_chr("id")

  call
}

#' @param repo The url of the repo, using https
#' @param local_path The path in which to store the git repo
#' @rdname load_call
#' @export
load_call_git <- function(
  repo,
  local_path = fs::path(".certigo/repos", digest::digest(repo, "md5")),
  call_path = "workflow.R",
  id = repo,
  ...
) {
  pull_or_clone(repo, local_path)

  id <- gsub("https://github.com/", "", id)

  load_call(path_workflow(fs::path(local_path, call_path)), id = id, ...)
}



pull_or_clone <- function(repo, local_path) {
  if (fs::dir_exists(local_path)) {
    git2r::pull(path_workflow(local_path))
  } else {
    git2r::clone(repo, local_path = path_workflow(local_path))
  }
}
