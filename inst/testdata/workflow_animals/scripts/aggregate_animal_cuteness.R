library(readr)
library(dplyr)
library(purrr)

# animal_cutenesses <- jsonlite::read_json(inputs[["animal_cutenesses"]])

map(inputs[["animal_cuteness_individual"]], read_csv) %>%
  bind_rows() %>%
  write_csv(outputs[[1]])
