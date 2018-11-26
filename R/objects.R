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
    write_history = function(call_digest) {stop("Write history not implemented for", self$id)},
    validate = function(design) {
      # run inside workflow directory
      withr::with_dir(
        path_workflow(),
        # either return TRUE, or a character vector containing what is not correct
        result <- tryCatch({
          self$valid(design)
          TRUE
        },
        error = function(e) {
          e$message
        },
        warning = function(e) {
          e$message
        }
        )
      )

      result
    },
    valid = function(design) {TRUE}
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
    },
    validate = function(design) {
      TRUE
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
    history_path = NULL,
    initialize = function(path) {
      if (is.null(path)) {stop("Path cannot be null")}

      path <- fs::path_norm(path) # cleanup path (eg. remove "//", or remove "./")

      self$path <- path
      self$id <- path
      self$string <- path
      self$history_path <- history_path(path)

      dir_create(path_workflow(fs::path_dir(path)), recursive = TRUE)
    },
    valid = function(design) {
      validate(fs::file_exists(path_workflow(self$path)), "File exists")
      super$valid()
    },
    read_history = function() {
      jsonlite::read_json(path_workflow(self$history_path), simplifyVector = TRUE)
    },
    write_history = function(...) {
      history <- list(
        digest = self$digest,
        modification_time = self$modification_time
      )
      history <- list_modify(history, ...)
      jsonlite::write_json(history, path_workflow(history_path(self$path)))
    },
    delete = function() {
      if (
        fs::file_exists(path_workflow(self$path)) &&
        fs::is_file(path_workflow(self$path))
      ) {
        fs::file_delete(path_workflow(self$path))
      }

      if (self$exists_history) file_delete(path_workflow(history_path(self$path)))
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
      digest <- NULL
      if (self$exists_history) {
        history <- self$read_history()
        if (as.character(self$modification_time) == history[["modification_time"]]) {
          digest <- history[["digest"]]
        }
      }

      if (is.null(digest)) {
        digest <- processx::run("md5sum", path_workflow(path))$stdout %>% gsub("([^ ]*).*", "\\1", .) %>% trimws()
      }

      digest
    },
    modification_time = function() {
      fs::file_info(path_workflow(self$path))$modification_time
    },
    exists = function() {
      fs::file_exists(path_workflow(self$path))
    },
    exists_history = function() {
      fs::file_exists(path_workflow(self$history_path))
    }
  )
)

DerivedFile <- R6Class(
  "DerivedFile",
  inherit = File,
  public = list(
    initialize = function(path) {
      # add derived file directory to path if defined (when using modules)
      # TODO: change this to a more clear "module_derived_directory" option
      path <- fs::path(getOption("derived_file_directory", default = "./"), path)

      super$initialize(path)

      # check the history if the derived file already exists
      if (fs::file_exists(path_workflow(self$path))) {
        if(!self$exists_history) {
          cat_line(crayon_warning("\U26A0 No history present for derived file:",  crayon::italic(self$path), ", deleting the file."))
          self$delete()
        } else {
          history <- self$read_history()
          if (!"call_digest" %in% names(history)) {
            cat_line(crayon_warning("\U26A0 No call digest present for derived file:", crayon::italic(self$path), ", deleting the file."))
            self$delete()
          }
        }
      } else if (self$exists_history) {
        cat_line(crayon_warning("\U26A0 History present, but not the derived file:",  crayon::italic(self$path), ", deleting the file."))
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
      # add workflow directory to path if defined (when using modules)
      # TODO: change this to a more clear "module_raw_directory" option
      path <- fs::path(getOption("workflow_directory", default = "./"), path)

      super$initialize(path)

      # check if raw file exists
      if (!file_exists(path_workflow(path))) {
        stop("\U274C Raw file does not exist: ",  crayon::italic(path))
      }

      # if the file is not within the current working directory, place it in the .certigo folder
      # as to make sure it gets mounted inside containers
      if (!path_is_child(path, ".")) {
        new_path <- path_workflow(".certigo/files/", self$digest)
        if (!fs::file_exists(path_workflow(new_path))) {
          fs::dir_create(path_workflow(fs::path_dir(new_path)), recursive = TRUE)
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





DerivedDirectory <- R6Class(
  "DerivedDirectory",
  inherit = DerivedFile,
  public = list(
    initialize = function(path) {
      # make sure the path of this folder ends with "/." so that fs::path_dir will retain the directory
      path <- fs::path(fs::path_norm(path), "/.")
      super$initialize(path)
    }
  ),
  active = list(
    digest = function(..., path = self$path) {
      # very dirt way to get a "digest" of a path
      # we should actually look at file contents here...
      # https://unix.stackexchange.com/questions/35832/how-do-i-get-the-md5-sum-of-a-directorys-contents-as-one-sum
      fs::dir_info(path_workflow(path), recursive = TRUE) %>%
        select(path, size) %>%
        arrange(size) %>%
        digest::digest("md5")
    },
    modification_time = function() {
      fs::dir_info(path_workflow(path), recursive = TRUE) %>%
        pull(modification_time) %>%
        max()
    }
  )
)

#' @rdname object
#' @export
derived_directory <- DerivedDirectory$new












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
      if (!fs::file_exists(path_workflow(parameters_file))) {
        dir_create(path_workflow(path_dir(parameters_file)), recursive = TRUE)
        jsonlite::write_json(parameters, path_workflow(parameters_file))
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



Atomic <- R6Class(
  "Atomic",
  inherit = Object,
  public = list(
    atomic = NULL,
    initialize = function(atomic) {
      self$atomic <- atomic
      self$string <- atomic
      self$id <- digest <- self$digest
    }
  ),
  active = list(
    digest = function(...) {
      digest::digest(self$atomic, algo = "md5")
    },
    exists = function(...) TRUE
  )
)


#' @rdname object
#' @export
atomic <- Atomic$new





ObjectSet <- R6Class(
  "ObjectSet",
  inherit = Object,
  public = list(
    objects = list(),
    initialize = function(objects) {
      # if all objects are atomic, create a set of parameters for each
      if (all(map_lgl(objects, is.atomic))) {
        objects <- map(objects, atomic)
      } else if (all(map_lgl(objects, ~"Object" %in% class(.)))) {

      } else {
        stop("Invalid set of objects, have class:", paste0(map(objects, class) %>% map_chr(first), collapse = ", "))
      }

      self$objects <- objects

      # get the strings of each object
      object_strings <- map(self$objects, "string")

      # simplify to a character vector if all objects are not sets themselves
      # else, all objects should be objects themselves
      if (!any(map_lgl(self$objects, ~"ObjectSet" %in% class(.)))) {
        object_strings <- as.character(object_strings)
      } else {
        testthat::expect_true(all(map_lgl(objects, ~"Object" %in% class(.))))
      }

      self$id <- digest::digest(object_strings, algo = "md5")
      self$string <- object_strings
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


path_is_child <- function(path, root = get_object_root()) {
  path_has_parent(path, root)
}
