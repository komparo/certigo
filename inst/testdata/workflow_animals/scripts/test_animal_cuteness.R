library(tibble)
library(dplyr)
library(tidyr)

animal_cuteness <- readr::read_tsv(inputs[1])

animal_cuteness_tests <- crossing(
  from = animal_cuteness$animal,
  to = animal_cuteness$animal
) %>%
  mutate(
    p_value = runif(n())
  )

readr::write_csv(animal_cuteness_tests, outputs[1])
