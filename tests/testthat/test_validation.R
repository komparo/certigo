context("Testing validation")

library(readr)
source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

object <- animal_cuteness("animal_cuteness.csv")

write_csv(
  tibble(
    animal = c("dog", "cat", "cat"),
    cuteness = c(1, 2, 0)
  ),
  "animal_cuteness.csv"
)

testthat::expect_true(object$validate())

write_csv(
  tibble(
    animal = c("dog", "cat", "cat"),
    cuteness = c(-1, 2, 0)
  ),
  "animal_cuteness.csv"
)

testthat::expect_error(object$valid(), info = "When wrong format, valid should error")
testthat::expect_true(is.character(object$validate()), "When wrong format, validate should return chracter")
