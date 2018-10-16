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
    history = NULL
  ),
  active = list(
    digest = function() {
      stop("Digest not implemented")
    },
    exists = function(...) {
      stop("Exists not implemented for ", self$id)
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
    },
    exists = function() {
      process <- processx::run("docker", c("inspect", "--format={{.ID}}", self$image), error_on_status = FALSE)
      process$status == 0
    }
  )
)


#' @rdname object
#' @export
docker <- Docker$new

history_path <- function(path) {
  paste0(
    path_dir(path),
    paste0("/.", path_file(path)),
    ".history"
  )
}

File <- R6Class(
  "File",
  inherit = Object,
  public = list(
    path = NULL,
    last_change_time = NULL,
    last_digest = NULL,
    initialize = function(path) {
      if (is.null(path)) {stop("Path cannot be null")}
      self$path <- path
      self$id <- path
      self$string <- path

      dir_create(path_dir(path), recursive = TRUE)
    },
    read_history = function() {
      jsonlite::read_json(history_path(self$path), simplifyVector = TRUE)
    },
    write_history = function(...) {
      history <- list(
        digest = self$digest,
        modification_time = self$modification_time
      )
      history <- list_modify(history, ...)
      jsonlite::write_json(history, history_path(self$path))
    },
    delete = function() {
      if (file_exists(self$path)) file_delete(self$path)
      if (self$exists_history) file_delete(history_path(self$path))
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
      processx::run("md5sum", self$path)$stdout %>% gsub("([^ ]*).*", "\\1", .) %>% trimws()
    },
    modification_time = function() {
      fs::file_info(self$path)$modification_time
    },
    exists = function() {
      file_exists(self$path)
    },
    exists_history = function() {
      file_exists(history_path(self$path))
    }
  )
)

DerivedFile <- R6Class(
  "DerivedFile",
  inherit = File,
  public = list(
    initialize = function(path) {
      super$initialize(path)

      # check the history if the derived file already exists
      if (file_exists(path)) {
        if(!self$exists_history) {
          cat_line(crayon_warning("\U26A0 No history present for derived file:",  crayon::italic(path), ", deleting the file."))
          self$delete()
        } else {
          history <- self$read_history()
          if (!"call_digest" %in% names(history)) {
            cat_line(crayon_warning("\U26A0 No call digest present for derived file:", crayon::italic(path), ", deleting the file."))
            self$delete()
          }
        }
      } else if (self$exists_history) {
        cat_line(crayon_warning("\U26A0 History present, but not the derived file:",  crayon::italic(path), ", deleting the file."))
        self$delete()
      }
    }
  ),
  active = list(
    call_digest = function(...) {
      if (self$exists_history) {
        self$read_history()$call_digest
      } else {
        NA
      }
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

      if (!file_exists(path)) {
        stop("\U274C Raw file does not exist yet: ",  crayon::italic(path))
      }

      # add history if it does not exist, otherwise check whether the history is up to date
      if (!self$exists_history) {
        message("\U23F0 Adding ", crayon::italic(path))
        self$write_history()
      } else {
        history <- self$read_history()
        if (history$modification_time != self$modification_time) {
          self$write_history()
        }
      }
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






Parameters <- R6Class(
  "Parameters",
  inherit = Object,
  public = list(
    parameters = NULL,
    initialize = function(parameters) {
      # sort if named, so that even when the order changes, the hash will stay the same
      if(!is.null(names(parameters))) {
        parameters <- parameters[sort(names(parameters))]
      }

      self$parameters <- parameters
      self$id <- digest <- self$digest

      parameters_file <- paste0("./.certigo/parameters/", digest)
      if (!file.exists(parameters_file)) {
        dir_create(path_dir(parameters_file), recursive = TRUE)
        jsonlite::write_json(parameters, parameters_file)
      }
      self$string <- parameters_file
    }
  ),
  active = list(
    digest = function(...) {
      digest::digest(self$parameters, algo = "md5")
    },
    exists = function(...) TRUE
  )
)


#' @rdname object
#' @export
parameters <- Parameters$new
