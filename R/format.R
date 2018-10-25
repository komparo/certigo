col_split <- function(begin, end, middle = " | ", width_begin = 40) {
  if(nchar(begin) > width_begin) {
    begin <- paste0(
      crayon::col_substring(begin, 1, floor(width_begin/2) - 1),
      "...",
      crayon::col_substring(begin, nchar(begin)-floor(width_begin/2))
    )
  }
  paste0(crayon::col_align(begin, width_begin, "right"), middle, end)
}

crayon_warning <- crayon::make_style("orange")
crayon_ok <- crayon::make_style("green")
crayon_error <- crayon::make_style("red")
crayon_info <- crayon::make_style("blue")
