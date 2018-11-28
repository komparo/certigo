# get the default scheduler:
# - if there is no

get_default_scheduler <- function() {
  getOption("certigo_scheduler", local_scheduler())
}
