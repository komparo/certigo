library(readr)
library(dplyr)
library(purrr)

animal_cutenesses <- jsonlite::read_json(inputs[["animal_cutenesses"]])

map(animal_cutenesses, read_csv) %>%
  bind_rows() %>%
  write_csv(outputs[[1]])
