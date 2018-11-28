library(readr)
library(dplyr)
library(purrr)

map(inputs[["animal_cuteness_individual"]], read_csv) %>%
  bind_rows() %>%
  write_csv(outputs[[1]])
