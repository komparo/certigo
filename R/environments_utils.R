# wraps the /usr/bin/time command to get resources
wrap_command_resources <- function(command, args, resources_file) {
  time_format <- '{"exit_code" : %x, "time_user_seconds" : %U, "time_system_seconds" : %S, "time_wall_clock_seconds" : %e, "rss_max_kbytes" : %M, "rss_avg_kbytes" : %t, "page_faults_major" : %F, "page_faults_minor" : %R, "io_inputs" : %I, "io_outputs" : %O, "context_switches_voluntary" : %w, "context_switches_involuntary" : %c, "cpu_percentage" : "%P", "signals_received" : %k}'
  args <- c("-o", resources_file, "-f", time_format, command, args)
  command <- "/usr/bin/time"

  list(command = command, args = args)
}




get_default_environment <- function() {
  getOption("certigo_environment", default = docker_environment("rocker/tidyverse"))
}
