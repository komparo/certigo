
#   ____________________________________________________________________________
#   FontAwesome                                                             ####

load_fontawesome <- function() {
  requireNamespace("extrafont")

  font_dir <- system.file("fonts/", package = "certigo")

  if (!"Font Awesome 5 Free" %in% extrafont::fonts()) {
    sink(tempfile())
    extrafont::font_import(paths = font_dir, pattern = "fa-solid-900\\.ttf", prompt = FALSE)
    extrafont::font_import(paths = font_dir, pattern = "fa-brands-400\\.ttf", prompt = FALSE)
    sink()
  }
}


#   ____________________________________________________________________________
#   Get root of objects                                                     ####

get_object_root <- function() {
  getOption("certigo_root", ".")
}


path_workflow <- function(...) {
  fs::path(get_object_root(), ...)
}
