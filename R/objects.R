#' Objects
#'
#' @param ... ...
#'
#' @rdname object

Object <- R6Class("Object", public = list(
  id = NULL,
  string = "",
  digest = function() {
    stop("Digest not implemented")
  }
))

DockerContainer <- R6Class(
  "DockerContainer",
  inherit = Object,
  public = list(
    image = NULL,
    initialize = function(image) {
      self$image <- image
      self$id <- image
      self$string <- path
    },
    digest = function() {
      process <- processx::run("docker", c("inspect", "--format={{.ID}}", self$image))

      process$stdout %>% trimws()
    }
  )
)


#' @rdname object
#' @export
docker_container <- DockerContainer$new

File <- R6Class(
  "File",
  inherit = Object,
  public = list(
    path = NULL,
    file_required = TRUE,
    initialize = function(path) {
      self$path <- path
      self$id <- path
      self$string <- path
    },
    digest = function() {
      if (self$file_required) {
        if (!file.exists(self$path)) {stop(glue::glue("{self$path} -> does not exist"))}
      } else {
        if (!file.exists(self$path)) {return(NA)}
      }

      processx::run("md5sum", self$path)$stdout %>% gsub("([^ ]*).*", "\\1", .) %>% trimws()
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
