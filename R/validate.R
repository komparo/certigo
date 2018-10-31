#' Add a validator to an existing object initialize function
#'
#' @param parent_initialize `derived_file`
#' @param validator_function Function which takes design as an argument
#' @export
add_validators <- function(parent_initialize, validator_function) {
  parent_class <- environment(parent_initialize)

  R6::R6Class(
    inherit = parent_class,
    public = list(
      valid = validator_function
    )
  )$new
}




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
