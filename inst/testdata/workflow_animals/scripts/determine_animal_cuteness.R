library(tibble)

design <- jsonlite::read_json(inputs[[1]], simplifyVector = TRUE)

animal_cuteness <- tibble(
  animal = design$animal,
  cuteness = rgamma(design$n_animals, 1, 1/design$cuteness_mean)
)

readr::write_csv(animal_cuteness, outputs[[1]])
