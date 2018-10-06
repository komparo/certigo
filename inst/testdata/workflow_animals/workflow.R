library(tidyverse)

calls_raw <- list(
  rscript_call(
    "determine_animal_coolness",
    script_file("scripts/determine_animal_coolness.R"), 
    outputs = list(derived_file("derived/animal_coolness.tsv"))
  ),
  rscript_call(
    "plot_animal_coolness",
    script_file("scripts/plot_animal_coolness.R"),
    inputs = list(derived_file("derived/animal_coolness.tsv")),
    outputs = list(derived_file("derived/animal_coolness.pdf"))
  )
)

process_calls_raw <- function(calls_raw) {
  set_names(calls_raw, map_chr(calls_raw, "id")) %>% enframe("id", "call")
}

calls <- process_calls_raw(calls_raw)
# runs_exited = tibble(id = character(), digest = character())

fs::file_delete(fs::dir_ls("derived"))
runs_exited <- run_calls(calls, runs_exited)

write_rds(runs_exited, "runs_exited.rds")
runs_exited <- read_rds("runs_exited.rds")
