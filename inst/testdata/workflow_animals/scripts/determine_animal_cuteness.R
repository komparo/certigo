library(tibble)

design <- jsonlite::read_json(inputs[1], simplifyVector = TRUE)

animal_cuteness <- tibble(
  animal = design$animal,
  cuteness = rnorm(design$n_animals, design$cuteness_mean, 0.2)
)

readr::write_csv(animal_cuteness, outputs[1])
