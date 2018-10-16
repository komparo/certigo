library(tibble)

design <- jsonlite::read_json(inputs[1])

animal_cuteness <- tibble(
  animal = design$animal,
  cuteness
)

readr::write_tsv(animal_cuteness, outputs[1])
