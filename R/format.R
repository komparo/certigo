col_split <- function(begin, end, middle = " | ", width_begin = 25) {
  paste0(crayon::col_align(crayon::col_substring(begin, 1, width_begin), width_begin, "right"), middle, end)
}

crayon_warning <- crayon::make_style("orange")
crayon_ok <- crayon::make_style("green")
crayon_error <- crayon::make_style("red")
