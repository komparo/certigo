
#   ____________________________________________________________________________
#   FontAwesome                                                             ####

load_fontawesome <- function() {
  requireNamespace("extrafont")

  font_dir <- system.file("inst/fonts/", package = "certigo")

  if (!"Font Awesome 5 Free" %in% extrafont::fonts()) {
    sink(tempfile())
    extrafont::font_import(paths = font_dir, pattern = "fa-solid-900\\.ttf", prompt = FALSE)
    extrafont::font_import(paths = font_dir, pattern = "fa-brands-400\\.ttf", prompt = FALSE)
    sink()
  }
}
