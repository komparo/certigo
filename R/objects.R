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
    history = NULL,
    write_history = function(call_digest) {stop("Write history not implemented for", self$id)}
  ),
  active = list(
    digest = function() {
      stop("Digest not implemented for", self$id)
    },
    exists = function(...) {
      stop("Exists not implemented for ", self$id)
    },
    call_digest = function(...) {
      stop("Call digest not implemented for ", self$id)
    },
    individual = function(...) {
      list(self)
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

      path <- fs::path_norm(path) # cleanup path (eg. remove "//", or remove "./")

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
    digest = function(..., path = self$path) {
      processx::run("md5sum", path)$stdout %>% gsub("([^ ]*).*", "\\1", .) %>% trimws()
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

      # check if raw file exists
      if (!file_exists(path)) {
        stop("\U274C Raw file does not exist: ",  crayon::italic(path))
      }

      # if the file is not within the current working directory, place it in the .certigo folder
      # as to make sure it gets mounted inside containers
      if (!path_is_child(path, ".")) {
        new_path <- paste0(".certigo/files/", self$digest)
        if (!fs::file_exists(new_path)) {
          fs::dir_create(fs::path_dir(new_path), recursive = TRUE)
          fs::file_copy(
            path,
            new_path
          )
        }
        path <- new_path
        self$string <- path
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












ObjectSet <- R6Class(
  "ObjectSet",
  inherit = Object,
  public = list(
    objects = list(),
    initialize = function(objects) {
      self$objects <- objects
      object_strings <- map_chr(self$objects, "string")
      self$id <- digest::digest(object_strings, algo = "md5")
      self$string <- object_strings
      # object_set_file <- paste0("./.certigo/object_sets/", self$id)
      # if (!file.exists(object_set_file)) {
      #   dir_create(path_dir(object_set_file), recursive = TRUE)
      #   jsonlite::write_json(object_strings, object_set_file)
      # }
      # self$string <- object_set_file
    }
  ),
  active = list(
    digest = function(...) {
      map_chr(self$objects, "digest") %>% digest::digest(algo = "md5")
    },
    exists = function(...) map_lgl(self$objects, "exists") %>% all(),
    individual = function(...) {
      self$objects %>% map("individual") %>% flatten()
    }
  )
)


#' @rdname object
#' @export
object_set <- ObjectSet$new







path_is_child <- function(path, start = ".") {
  startsWith(fs::path_abs(path), fs::path_real(start))
}
