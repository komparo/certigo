context("Testing different environments")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

certigo_tmp_file <- function() {
  if(!fs::dir_exists(path_workflow(".certigo/tmp"))) {
    fs::dir_create(path_workflow(".certigo/tmp"))
  }
  tempfile(tmpdir = ".certigo/tmp")
}

test_that("LocalEnvironment", {
  resources_file <- certigo_tmp_file()
  environment <- LocalEnvironment$new()
  environment$start("echo", c("hi"), resources_file = resources_file)
  environment$wait()

  testthat::expect_true(fs::file_exists(path_workflow(resources_file)))
})


test_that("DockerEnvironment", {
  # normal execution
  environment <- docker_environment("certigo/animal_cuteness")
  environment$start("echo", "hi")
  environment$wait()
  expect_true(environment$exists)
  expect_is(environment$digest, "character")

  # execution with resources
  resources_file <- certigo_tmp_file()
  environment <- docker_environment("certigo/animal_cuteness")
  environment$start("echo", "hi", resources_file = resources_file)
  environment$wait()

  expect_true(fs::file_exists(path_workflow(resources_file)))

  # non-existent image
  environment <- docker_environment("blabla")
  expect_false(environment$exists)
  expect_equal(environment$digest, "")
  environment$start("echo", "hi")
  environment$wait()
  expect_true(first(nchar(environment$error)) > 0)
})

