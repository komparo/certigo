library(testthat)
library(fs)

context("Testing workflow animals")

# go to temporary directory and copy over all files
create_workflow_dir <- function() {
  oldwd <- getwd()
  # on.exit({setwd(oldwd)})
  tempdir <- tempfile()
  dir.create(tempdir)
  dir_copy(system.file("testdata/workflow_animals", package = "certigo"), tempdir)
  setwd(paste0(tempdir, "/workflow_animals"))

  # recopy scripts
  file_copy(dir_ls(system.file("testdata/workflow_animals/scripts", package = "certigo")), "./scripts/", overwrite = T)
}

# build the docker image
processx::run("docker", c("build", "-t", "certigo/plot_animal_cuteness", system.file('testdata/workflow_animals/containers/plot_animal_cuteness/', package = 'certigo')), echo = F)

# some testing functions
expect_rerun <- function(x) {expect_output(x, "^.*Finished$", info = "Expected a rerun")}
expect_cached <- function(x) {expect_output(x, "^.*Cached$", info = "Expected a cached")}

# function to reset
create_workflow_dir()

# create individual calls and call sets, and combine into a workflow
source(system.file("testdata/workflow_animals/workflow.R", package = "certigo"))

##  ............................................................................
##  Test individual calls                                                   ####
expect_rerun(determine_animal_cuteness$start_and_wait())

# cached
expect_cached(determine_animal_cuteness$start_and_wait())

# deleted output -> rerun
determine_animal_cuteness$outputs$animal_cuteness[[1]]$delete()
expect_rerun(determine_animal_cuteness$start_and_wait())

# run with multiple inputs
expect_rerun(aggregate_animal_cuteness$start_and_wait())

# test docker execution
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
expect_rerun(test_animal_cuteness$start_and_wait())

expect_rerun(plot_animal_cuteness_tests$start_and_wait())

expect_error(capture_output(always_error$start_and_wait()))

##  ............................................................................
##  Test workflow                                                           ####
create_workflow_dir()
source(system.file("testdata/workflow_animals/workflow.R", package = "certigo"))

animal_workflow$reset()
expect_rerun(animal_workflow$run())
animal_workflow$reset()
expect_cached(animal_workflow$run())

##  ............................................................................
##  Cleanup                                                                 ####
setwd(system.file(package = "certigo"))
