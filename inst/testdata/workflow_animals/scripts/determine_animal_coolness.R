library(tibble)

animal_coolness <- tibble(
  animal = c("dog", "cat", "horse"),
  coolness = c(1, 0.9, 0.8)
)

readr::write_tsv(animal_coolness, outputs[1])
