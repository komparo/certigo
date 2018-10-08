library(testthat)
library(fs)
library(dplyr)
library(readr)

context("Testing workflow animals")

# go to temporary directory and copy over all files
oldwd <- getwd()
tempdir <- tempfile()
dir.create(tempdir)
dir_copy(system.file("testdata/workflow_animals", package = "certigo"), tempdir)
setwd(paste0(tempdir, "/workflow_animals"))

expect_rerun <- function(x) {expect_output(x, "^.*\n.*$", info = "Expected a rerun")}
expect_norerun <- function(x) {expect_output(x, "^[^\n]*$", info = "Expected a no rerun")}

# build initial workflow
workflow <- Workflow$new(list(
  rscript_call(
    "determine_animal_coolness",
    script_file("scripts/determine_animal_coolness.R"),
    outputs = list(derived_file("intermediate/animal_coolness.tsv"))
  ),
  rscript_call(
    "plot_animal_coolness",
    script_file("scripts/plot_animal_coolness.R"),
    inputs = list(derived_file("intermediate/animal_coolness.tsv")),
    outputs = list(derived_file("results/animal_coolness.pdf"))
  ),
  rscript_call(
    "test_animal_coolness",
    script_file("scripts/test_animal_coolness.R"),
    inputs = list(derived_file("intermediate/animal_coolness.tsv")),
    outputs = list(derived_file("intermediate/animal_coolness_tests.csv"))
  ),
  rscript_call(
    "plot_animal_coolness_tests",
    script_file("scripts/plot_animal_coolness_tests.R"),
    inputs = list(derived_file("intermediate/animal_coolness.tsv"), derived_file("intermediate/animal_coolness_tests.csv")),
    outputs = list(derived_file("results/animal_coolness_tests.pdf"))
  )
))

# workflow$run_calls()

# workflow$runs_exited$stderr[3] %>% cat


expect_rerun(workflow$run_calls())

test_that("Derived files are being created", {
  expect_true(file_exists("results/animal_coolness.pdf"))
  expect_true(file_exists("intermediate/animal_coolness.tsv"))
})

test_that("Nothing gets rerun when rerunning with runs_exited", {
  expect_norerun(workflow$run_calls())
})

test_that("When previously output dissapears or changes, need to rerun", {
  file_delete("results/animal_coolness.pdf")
  expect_rerun(workflow$run_calls())
  expect_true(file_exists("results/animal_coolness.pdf"))

  file_move("results/animal_coolness.pdf", "results/animal_coolness.png")
  expect_rerun(workflow$run_calls())
  expect_true(file_exists("results/animal_coolness.pdf"))
})

test_that("When an input or dissapears changes, need to rerun", {
  read_file("scripts/determine_animal_coolness.R") %>% gsub("dog", "giraffe", .) %>% write_file("scripts/determine_animal_coolness.R")
  expect_rerun(workflow$run_calls())

  file_delete("intermediate/animal_coolness.tsv")
  expect_rerun(workflow$run_calls())

  read_file("intermediate/animal_coolness.tsv") %>% gsub("cat", "meow", .) %>% write_file("intermediate/animal_coolness.tsv")
  expect_rerun(workflow$run_calls())
})

test_that("When workflow changes, need to rerun", {
  workflow <- Workflow$new(list(
    rscript_call(
      "determine_animal_coolness",
      script_file("scripts/determine_animal_coolness.R"),
      outputs = list(derived_file("derived/animal_coolness.tsv"))
    ),
    rscript_call(
      "plot_animal_coolness",
      script_file("scripts/plot_animal_coolness.R"),
      inputs = list(derived_file("derived/animal_coolness.tsv")),
      outputs = list(derived_file("derived/animal_coolness.png"))
    )
  ), runs_exited = workflow$runs_exited)

  expect_rerun(workflow$run_calls())
})

test_that("Workflow can be plotted", {
  expect_is(workflow$plot_workflow(), "ggplot")
})

setwd(oldwd)
