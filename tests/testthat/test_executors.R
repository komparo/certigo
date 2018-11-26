context("Testing different executors")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

test_that("ProcessExecutor", {
  resources_file <- certigo_tmp_file()
  executor <- ProcessExecutor$new()
  executor$start("echo", c("hi"), resources_file = resources_file)
  executor$wait()

  testthat::expect_true(fs::file_exists(path_workflow(resources_file)))
})


test_that("DockerExecutor", {
  resources_file <- certigo_tmp_file()
  executor <- DockerExecutor$new("certigo/plot_animal_cuteness")
  executor$start("echo", "hi", resources_file = resources_file)
  executor$wait()

  testthat::expect_true(fs::file_exists(path_workflow(resources_file)))
})

