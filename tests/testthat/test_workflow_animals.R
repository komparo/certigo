library(testthat)
library(fs)
library(dplyr)
library(readr)
library(purrr)

context("Testing workflow animals")

# go to temporary directory and copy over all files
oldwd <- getwd()
on.exit({setwd(oldwd)})
tempdir <- tempfile()
dir.create(tempdir)
dir_copy(system.file("testdata/workflow_animals", package = "certigo"), tempdir)
setwd(paste0(tempdir, "/workflow_animals"))

# build the docker image
processx::run("docker", c("build", "-t", "certigo/plot_animal_cuteness", system.file('testdata/workflow_animals/containers/plot_animal_cuteness/', package = 'certigo')), echo = T)

# some testing functions
expect_rerun <- function(x) {expect_output(x, "^.*Finished$", info = "Expected a rerun")}
expect_cached <- function(x) {expect_output(x, "^.*Cached$", info = "Expected a cached")}

# dir_delete("derived")
# file_delete(dir_ls(".", recursive = TRUE, glob = "*.history", all = TRUE))

# run with multiple outputs
design <- tibble(
  animal = c("dog", "cat", "horse"),
  cuteness_mean = c(1, 0.9, 0.8),
  n_animals = c(10, 20, 30)
)

determine_animal_cuteness <- rscript_call(
  "determine_animal_cuteness",
  script_file("scripts/determine_animal_cuteness.R"),
  outputs = list(derived_file(stringr::str_glue("derived/animal_cuteness/{animal}.csv"))),
  design = design
)

expect_rerun(determine_animal_cuteness$start())

# cached
expect_cached(determine_animal_cuteness$start())

# deleted output -> rerun
determine_animal_cuteness$calls[[1]]$outputs[[1]]$delete()
expect_rerun(determine_animal_cuteness$start())

# run with multiple inputs
aggregate_animal_cuteness <- rscript_call(
  "aggregate_animal_cuteness",
  script_file("scripts/aggregate_animal_cuteness.R"),
  inputs = stringr::str_glue("derived/animal_cuteness/{design$animal}.csv") %>% map(derived_file),
  outputs = list(derived_file("derived/animal_cuteness.csv"))
)

expect_rerun(aggregate_animal_cuteness$start_and_wait())

# test docker execution
plot_animal_cuteness <- rscript_call(
  "plot_animal_cuteness",
  script_file("scripts/plot_animal_cuteness.R"),
  inputs = list(derived_file("derived/animal_cuteness.csv")),
  outputs = list(derived_file("results/animal_cuteness.pdf")),
  executor = docker_executor("certigo/plot_animal_cuteness")
)

expect_rerun(plot_animal_cuteness$start_and_wait())
expect_true(file_exists(plot_animal_cuteness$outputs[[1]]$string))

# no input -> error + deleted output
aggregate_animal_cuteness$outputs[[1]]$delete()
expect_error(plot_animal_cuteness$start_and_wait())
expect_false(file_exists(plot_animal_cuteness$outputs[[1]]$string))

# input -> rerun
aggregate_animal_cuteness$start_and_wait()
expect_rerun(plot_animal_cuteness$start_and_wait())

# test some other calls
test_animal_cuteness <- rscript_call(
  "test_animal_cuteness",
  script_file("scripts/test_animal_cuteness.R"),
  inputs = list(derived_file("derived/animal_cuteness.csv")),
  outputs = list(derived_file("derived/animal_cuteness_tests.csv"))
)

expect_rerun(test_animal_cuteness$start_and_wait())

plot_animal_cuteness_tests <- rscript_call(
  "plot_animal_cuteness_tests",
  script_file("scripts/plot_animal_cuteness_tests.R"),
  inputs = list(derived_file("derived/animal_cuteness.csv"), derived_file("derived/animal_cuteness_tests.csv")),
  outputs = list(derived_file("results/animal_cuteness_tests.pdf"))
)

expect_rerun(plot_animal_cuteness_tests$start_and_wait())

setwd(oldwd)
# setwd(system.file(package = "certigo"))
