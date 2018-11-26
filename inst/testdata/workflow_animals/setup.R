library(testthat)
library(fs)

# go to temporary directory and copy over all files
create_workflow_dir <- function() {
  tempdir <- tempfile()
  dir.create(tempdir)
  dir_copy(system.file("testdata/workflow_animals", package = "certigo"), tempdir)

  options(certigo_root = paste0(tempdir, "/workflow_animals"))
}

# build the docker image

# some testing functions
expect_rerun <- function(x) {expect_output(x, "^.*Finished.*$", info = "Expected a rerun")}
expect_rerun_somewhere <- function(x) {expect_output(x, ".*Finished.*", info = "Expected a rerun")}
expect_cached <- function(x) {expect_output(x, "^.*Cached.*$", info = "Expected a cached")}
expect_cached_somewhere <- function(x) {expect_output(x, ".*Cached.*", info = "Expected a cached")}

create_workflow_dir()

source(system.file("testdata/workflow_animals/workflow.R", package = "certigo"))
