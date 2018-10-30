library(testthat)
library(fs)

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
expect_rerun <- function(x) {expect_output(x, "^.*Finished.*$", info = "Expected a rerun")}
expect_rerun_somewhere <- function(x) {expect_output(x, ".*Finished.*", info = "Expected a rerun")}
expect_cached <- function(x) {expect_output(x, "^.*Cached.*$", info = "Expected a cached")}
expect_cached_somewhere <- function(x) {expect_output(x, ".*Cached.*", info = "Expected a cached")}

create_workflow_dir()

source(system.file("testdata/workflow_animals/workflow.R", package = "certigo"))
