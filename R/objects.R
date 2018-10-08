#' Objects
#'
#' @param ... ...
#'
#' @rdname object

Object <- R6Class(
  "Object",
  public = list(
    id = NULL,
    string = NULL,
    status = function() {
      case_when(
        is.na(self$digest) ~ "not_present",
        TRUE ~ "present"
      )
    }
  ),
  active = list(
    digest = function() {
      stop("Digest not implemented")
    }
  )
)

Docker <- R6Class(
  "Docker",
  inherit = Object,
  public = list(
    image = NULL,
    initialize = function(image) {
      self$image <- image
      self$id <- image
      self$string <- image
    }
  ),
  active = list(
    label = function(...) fontawesome_map["box"],
    digest = function() {
      process <- processx::run("docker", c("inspect", "--format={{.ID}}", self$image))

      process$stdout %>% trimws()
    }
  )
)


#' @rdname object
#' @export
docker <- Docker$new

File <- R6Class(
  "File",
  inherit = Object,
  public = list(
    path = NULL,
    last_change_time = NULL,
    last_digest = NULL,
    file_required = TRUE,
    initialize = function(path) {
      self$path <- path
      self$id <- path
      self$string <- path

      dir_create(path_dir(path), recursive = TRUE)
    }
  ),
  active = list(
    label = function(...) {
      case_when(
        any(endsWith(self$path, c("png", "svg"))) ~ fontawesome_map["image"],
        any(endsWith(self$path, c("tsv", "csv"))) ~ fontawesome_map["table"],
        any(endsWith(self$path, c(".R"))) ~ paste0(fontawesome_map["code"]),
        any(endsWith(self$path, c(".pdf"))) ~ paste0(fontawesome_map["file-pdf"]),
        TRUE ~ fontawesome_map["file"]
      )
    },
    digest = function(...) {
      # check if file is present if required (eg. for raw files)
      if (self$file_required) {
        if (!file.exists(self$path)) {stop(glue::glue("{self$path} -> does not exist"))}
      } else {
        if (!file.exists(self$path)) {return(NA)}
      }

      # use change time to cache result
      current_change_time <- fs::file_info(self$path)$change_time
      if (is.null(self$last_change_time) || current_change_time > self$last_change_time) {
        self$last_digest <- processx::run("md5sum", self$path)$stdout %>% gsub("([^ ]*).*", "\\1", .) %>% trimws()
      }
      self$last_change_time <- current_change_time
      self$last_digest
    }
  )
)

DerivedFile <- R6Class(
  "DerivedFile",
  inherit = File,
  public = list(
    file_required = FALSE,
    initialize = function(path) {
      super$initialize(path)
    }
  )
)

#' @rdname object
#' @export
derived_file <- DerivedFile$new


RawFile <- R6Class(
  "RawFile",
  inherit = File,
  public = list(
    initialize = function(path) {
      super$initialize(path)
    }
  )
)

#' @rdname object
#' @export
raw_file <- RawFile$new


ScriptFile <- R6Class(
  "ScriptFile",
  inherit = RawFile,
  public = list(
    initialize = function(path) {
      super$initialize(path)
    }
  )
)

#' @rdname object
#' @export
script_file <- ScriptFile$new
