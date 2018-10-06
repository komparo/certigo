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
calls <- list(
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
) %>% process_calls_raw()


if (Sys.getenv("R_TESTS") != "") {
  expect_rerun(runs_exited <- run_calls(calls))

  test_that("Derived files are being created", {
    expect_true(file_exists("derived/animal_coolness.png"))
    expect_true(file_exists("derived/animal_coolness.tsv"))
  })

  test_that("Nothing gets rerun when rerunning with runs_exited", {
    expect_norerun(run_calls(calls, runs_exited))
  })

  test_that("When previously output dissapears or changes, need to rerun", {
    file_delete("derived/animal_coolness.png")
    expect_rerun(runs_exited <- run_calls(calls, runs_exited))
    expect_true(file_exists("derived/animal_coolness.png"))

    file_move("derived/animal_coolness.png", "derived/animal_coolness.pom")
    expect_rerun(runs_exited <- run_calls(calls, runs_exited))
    expect_true(file_exists("derived/animal_coolness.png"))
  })

  test_that("When an input or dissapears changes, need to rerun", {
    read_file("scripts/determine_animal_coolness.R") %>% gsub("dog", "giraffe", .) %>% write_file("scripts/determine_animal_coolness.R")
    expect_rerun(runs_exited <- run_calls(calls, runs_exited))

    file_delete("derived/animal_coolness.tsv")
    expect_rerun(runs_exited <- run_calls(calls, runs_exited))

    read_file("derived/animal_coolness.tsv") %>% gsub("cat", "meow", .) %>% write_file("derived/animal_coolness.tsv")
    expect_rerun(runs_exited <- run_calls(calls, runs_exited))
  })

  test_that("When workflow changes, need to rerun", {
    calls <- list(
      rscript_call(
        "determine_animal_coolness",
        script_file("scripts/determine_animal_coolness.R"),
        outputs = list(derived_file("derived/animal_coolness.tsv"))
      ),
      rscript_call(
        "plot_animal_coolness",
        script_file("scripts/plot_animal_coolness.R"),
        inputs = list(derived_file("derived/animal_coolness.tsv")),
        outputs = list(derived_file("derived/animal_coolness.pdf"))
      )
    ) %>% process_calls_raw()

    expect_rerun(runs_exited <- run_calls(calls, runs_exited))
  })


  # write_rds(runs_exited, "runs_exited.rds")
  # runs_exited <- read_rds("runs_exited.rds")

  setwd(oldwd)

}
