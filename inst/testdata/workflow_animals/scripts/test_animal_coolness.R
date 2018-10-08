library(tibble)
library(dplyr)
library(tidyr)

args <- commandArgs(trailingOnly = TRUE)
path_input <- args[1]
path_output <- args[2]

animal_coolness <- readr::read_tsv(path_input)

animal_coolness_tests <- crossing(
  from = animal_coolness$id,
  to = animal_coolness$id
) %>%
  mutate(
    p_value = runif(n())
  )

readr::write_csv(animal_coolness_tests, path_output)
