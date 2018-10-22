library(readr)
library(dplyr)
library(purrr)

map(inputs[2:length(inputs)], read_csv) %>%
  bind_rows() %>%
  write_csv(outputs[[1]])
