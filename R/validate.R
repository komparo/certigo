# Returns TRUE if expression is TRUE
# Returns info otherwise
validate <- function(expr, info = NULL) {
  expr <- enquo(expr)
  if (is.null(info)) {
    info <- deparse_friendly(expr)
  }
  result <- rlang::eval_tidy(expr)
  if (!isTRUE(result)) {
    stop(info, call. = FALSE)
  }
}
