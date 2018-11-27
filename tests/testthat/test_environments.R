context("Testing different schedulers and environments")

certigo_tmp_file <- function() {
  if(!fs::dir_exists(path_workflow(".certigo/tmp"))) {
    fs::dir_create(path_workflow(".certigo/tmp"))
  }
  tempfile(tmpdir = ".certigo/tmp")
}

scheduler <- local_scheduler()

# make sure there is always only one local schedular
expect_equal(scheduler, local_scheduler())
expect_equal(scheduler, getOption("certigo_local_scheduler"))

test_that("LocalScheduler + LocalEnvironment", {
  environment <- local_environment()

  # normal execution
  expect_true(scheduler$status(NULL) == "waiting")
  job_id <- scheduler$start("sleep", "0.5", environment = environment)
  expect_true(scheduler$status(job_id) == "running")
  output <- scheduler$wait(job_id)
  expect_true(scheduler$status(job_id) == "success")

  # with resource file
  resources_file <- certigo_tmp_file()
  job_id <- scheduler$start("echo", c("hi"), resources_file = resources_file)
  output <- scheduler$wait(job_id)
  expect_true(fs::file_exists(path_workflow(resources_file)))
})

test_that("LocalScheduler + DockerEnvironment", {
  environment <- docker_environment()

  # normal execution
  expect_true(scheduler$status(NULL) == "waiting")
  job_id <- scheduler$start("sleep", "0.5", environment = environment)
  expect_true(scheduler$status(job_id) == "running")
  output <- scheduler$wait(job_id)
  expect_true(scheduler$status(job_id) == "success")

  # with resource file
  resources_file <- certigo_tmp_file()
  job_id <- scheduler$start("echo", c("hi"), resources_file = resources_file)
  output <- scheduler$wait(job_id)
  expect_true(fs::file_exists(path_workflow(resources_file)))

  # non-existent image
  environment <- docker_environment("blabla")
  expect_false(environment$exists)
  expect_equal(environment$digest, "")
  job_id <- scheduler$start("sleep", "0.5", environment = environment)
  output <- scheduler$wait(job_id)
  expect_true(output$status == "errored")
  expect_true(first(nchar(output$error)) > 0)
})
