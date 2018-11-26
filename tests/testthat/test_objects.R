context("Testing objects")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

test_that("Raw files", {
  temp_file <- tempfile(tmpdir = path_workflow("raw")) %>% fs::path_rel(path_workflow())
  readr::write_csv(tibble::tibble(x = 1:2), path_workflow(temp_file))

  file <- raw_file(temp_file)
  expect_true(file$exists)
  expect_is(file$digest, "character")
})



test_that("Derived files", {
  temp_file <- tempfile(tmpdir = path_workflow("derived")) %>% fs::path_rel(path_workflow())

  # no history
  readr::write_csv(tibble::tibble(x = 1:2), path_workflow(temp_file))
  expect_output(derived_file(temp_file), "No history present")

  # with history
  file <- derived_file(temp_file)
  expect_false(file$exists)
  expect_error(expect_warning(file$read_history()))

  readr::write_csv(tibble::tibble(x = 1:2), path_workflow(temp_file))
  file$write_history(hi = "hi")

  expect_true(!is.null(file$read_history()))
  expect_equal(file$read_history()$hi, "hi")
  expect_true("digest" %in% names(file$read_history()))
  expect_true("modification_time" %in% names(file$read_history()))
})
