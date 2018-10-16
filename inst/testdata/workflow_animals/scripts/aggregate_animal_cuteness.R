library(readr)
library(dplyr)
library(purrr)

map(inputs, read_csv) %>%
  bind_rows() %>%
  write_csv(outputs[[1]])
