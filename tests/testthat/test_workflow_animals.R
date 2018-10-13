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
processx::run("docker", c("build", "-t", "certigo/plot_animal_coolness", system.file('testdata/workflow_animals/containers/plot_animal_coolness/', package = 'certigo')))

# some testing functions
expect_rerun <- function(x) {expect_output(x, "^.*Finished$", info = "Expected a rerun")}
expect_cached <- function(x) {expect_output(x, "^.*Cached$", info = "Expected a cached")}

# dir_delete("derived")
# file_delete(dir_ls(".", recursive = TRUE, glob = "*.history", all = TRUE))

determine_animal_coolness <- rscript_call(
  "determine_animal_coolness",
  script_file("scripts/determine_animal_coolness.R"),
  outputs = list(derived_file("derived/animal_coolness.tsv"))
)

expect_rerun(determine_animal_coolness$run())

# cached
expect_cached(determine_animal_coolness$run())

# deleted output -> rerun
determine_animal_coolness$outputs[[1]]$delete()
expect_rerun(determine_animal_coolness$run())

# test docker call
plot_animal_coolness <- docker_call(
  "plot_animal_coolness",
  docker("certigo/plot_animal_coolness"),
  inputs = list(derived_file("derived/animal_coolness.tsv")),
  outputs = list(derived_file("results/animal_coolness.pdf"))
)

expect_rerun(plot_animal_coolness$run())
expect_true(file_exists(plot_animal_coolness$outputs[[1]]$string))

# no input -> error + deleted output
determine_animal_coolness$outputs[[1]]$delete()
expect_error(plot_animal_coolness$run())
expect_false(file_exists(plot_animal_coolness$outputs[[1]]$string))

# input -> rerun
determine_animal_coolness$run()
expect_rerun(plot_animal_coolness$run())

# test some other calls
test_animal_coolness <- rscript_call(
  "test_animal_coolness",
  script_file("scripts/test_animal_coolness.R"),
  inputs = list(derived_file("derived/animal_coolness.tsv")),
  outputs = list(derived_file("derived/animal_coolness_tests.csv"))
)

expect_rerun(test_animal_coolness$run())

plot_animal_coolness_tests <- rscript_call(
  "plot_animal_coolness_tests",
  script_file("scripts/plot_animal_coolness_tests.R"),
  inputs = list(derived_file("derived/animal_coolness.tsv"), derived_file("derived/animal_coolness_tests.csv")),
  outputs = list(derived_file("results/animal_coolness_tests.pdf"))
)

expect_rerun(plot_animal_coolness_tests$run())

setwd(oldwd)
