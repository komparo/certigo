library(tibble)
library(dplyr)
library(tidyr)

animal_coolness <- readr::read_tsv(inputs[1])

animal_coolness_tests <- crossing(
  from = animal_coolness$id,
  to = animal_coolness$id
) %>%
  mutate(
    p_value = runif(n())
  )

readr::write_csv(animal_coolness_tests, outputs[1])
