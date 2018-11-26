context("Testing different executors")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

certigo_tmp_file <- function() {
  if(!fs::dir_exists(path_workflow(".certigo/tmp"))) {
    fs::dir_create(path_workflow(".certigo/tmp"))
  }
  tempfile(tmpdir = ".certigo/tmp")
}

test_that("ProcessExecutor", {
  resources_file <- certigo_tmp_file()
  executor <- ProcessExecutor$new()
  executor$start("echo", c("hi"), resources_file = resources_file)
  executor$wait()

  testthat::expect_true(fs::file_exists(path_workflow(resources_file)))
})


test_that("DockerExecutor", {
  # normal execution
  executor <- docker_executor("certigo/animal_cuteness")
  executor$start("echo", "hi")
  executor$wait()
  expect_true(executor$exists)
  expect_is(executor$digest, "character")

  # execution with resources
  resources_file <- certigo_tmp_file()
  executor <- docker_executor("certigo/animal_cuteness")
  executor$start("echo", "hi", resources_file = resources_file)
  executor$wait()

  expect_true(fs::file_exists(path_workflow(resources_file)))

  # non-existent image
  executor <- docker_executor("blabla")
  expect_false(executor$exists)
  expect_equal(executor$digest, "")
  executor$start("echo", "hi")
  executor$wait()
  expect_true(first(nchar(executor$error)) > 0)
})

