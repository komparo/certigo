library(testthat)
library(fs)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

context("Testing workflow animals")

# go to temporary directory and copy over all files
oldwd <- getwd()
on.exit({setwd(oldwd)})
tempdir <- tempfile()
dir.create(tempdir)
dir_copy(system.file("testdata/workflow_animals", package = "certigo"), tempdir)
setwd(paste0(tempdir, "/workflow_animals"))

# recopy scripts
file_copy(dir_ls(system.file("testdata/workflow_animals/scripts", package = "certigo")), "./scripts/", overwrite = T)

# build the docker image
processx::run("docker", c("build", "-t", "certigo/plot_animal_cuteness", system.file('testdata/workflow_animals/containers/plot_animal_cuteness/', package = 'certigo')), echo = F)

# some testing functions
expect_rerun <- function(x) {expect_output(x, "^.*Finished$", info = "Expected a rerun")}
expect_cached <- function(x) {expect_output(x, "^.*Cached$", info = "Expected a cached")}

# dir_delete("derived")
# file_delete(dir_ls(".", recursive = TRUE, glob = "*.history", all = TRUE))

# define starting design
design <- tibble(
  animal = c("dog", "cat", "horse"),
  cuteness_mean = c(1, 0.9, 0.8),
  n_animals = c(10, 20, 30)
)

determine_animal_cuteness <- rscript_call(
  "determine_animal_cuteness",
  inputs = design %>% transmute(
    parameters = design %>% dynutils::mapdf(parameters),
    script = list(script_file("scripts/determine_animal_cuteness.R"))
  ),
  outputs = design %>%
    transmute(animal_cuteness = str_glue("derived/animal_cuteness/{design$animal}.csv") %>%
             map(derived_file)
    )
)

expect_rerun(determine_animal_cuteness$start_and_wait())

# cached
expect_cached(determine_animal_cuteness$start_and_wait())

# deleted output -> rerun
determine_animal_cuteness$outputs$animal_cuteness[[1]]$delete()
expect_rerun(determine_animal_cuteness$start_and_wait())

# run with multiple inputs
aggregate_animal_cuteness <- rscript_call(
  "aggregate_animal_cuteness",
  inputs = tibble(
    script = list(script_file("scripts/aggregate_animal_cuteness.R")),
    animal_cutenesses = list(object_set(determine_animal_cuteness$outputs$animal_cuteness))
  ),
  outputs = tibble(animal_cuteness = list(derived_file("derived/animal_cuteness.csv")))
)

expect_rerun(aggregate_animal_cuteness$start_and_wait())

# test docker execution
plot_animal_cuteness <- rscript_call(
  "plot_animal_cuteness",
  inputs = aggregate_animal_cuteness$outputs %>%
    mutate(
      script = list(script_file("scripts/plot_animal_cuteness.R")),
      executor = list(docker_executor("certigo/plot_animal_cuteness"))
    ),
  outputs = list(
    plot = derived_file("results/animal_cuteness.pdf")
  )
)

expect_rerun(plot_animal_cuteness$start_and_wait())
expect_true(file_exists(plot_animal_cuteness$outputs$plot[[1]]$string))

# no input -> error + deleted output
aggregate_animal_cuteness$outputs$animal_cuteness[[1]]$delete()
expect_error(capture_output(plot_animal_cuteness$start_and_wait()))
expect_false(file_exists(plot_animal_cuteness$outputs$plot[[1]]$string))

# input is again present -> rerun
expect_rerun(aggregate_animal_cuteness$start_and_wait())
expect_rerun(plot_animal_cuteness$start_and_wait())

# test some other calls
test_animal_cuteness <- rscript_call(
  "test_animal_cuteness",
  inputs = list(
    script = script_file("scripts/test_animal_cuteness.R"),
    animal_cuteness = derived_file("derived/animal_cuteness.csv")
  ),
  outputs = list(tests = derived_file("derived/animal_cuteness_tests.csv"))
)

expect_rerun(test_animal_cuteness$start_and_wait())

plot_animal_cuteness_tests <- rscript_call(
  "plot_animal_cuteness_tests",
  inputs = bind_cols(
    aggregate_animal_cuteness$outputs,
    test_animal_cuteness$outputs
  ) %>% mutate(
    script = list(script_file("scripts/plot_animal_cuteness_tests.R"))
  ),
  outputs = list(plot = derived_file("results/animal_cuteness_tests.pdf"))
)

expect_rerun(plot_animal_cuteness_tests$start_and_wait())

setwd(oldwd)
# setwd(system.file(package = "certigo"))
