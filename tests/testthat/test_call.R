context("Testing individual call")

source(system.file("testdata/simple/setup.R", package = "certigo"))

# create raw files
fs::dir_create(path_workflow("raw"))
jsonlite::write_json(10, path_workflow("raw/n.json"))

test_that("Rscript calls", {
  design <- list(
    script = script_file("scripts/rnorm.R"),
    n = raw_file("raw/n.json"),
    sample = derived_file("derived/sample.json")
  )

  call <- rscript_call(
    "rnorm",
    design = design,
    inputs = exprs(script, n),
    outputs = exprs(sample)
  )

  expect_rerun(call$start_and_wait())

  # with docker executor and resources
  call <- rscript_call(
    "rnorm",
    design = c(design, list(
      executor = docker_executor(container = "certigo/animal_cuteness"),
      resources = derived_file("derived/resources.json")
    )),
    inputs = exprs(script, n, executor),
    outputs = exprs(sample, resources)
  )

  expect_rerun(call$start_and_wait())

  resources <- jsonlite::read_json(path_workflow(call$design$resources[[1]]$path))
  expect_true(is.list(resources))

  # with default executor
  options(certigo_executor = local_executor())
  call <- rscript_call(
    "rnorm",
    design = design,
    inputs = exprs(script, n),
    outputs = exprs(sample)
  )
  expect_is(call$calls[[1]]$executor, "LocalExecutor")
  expect_rerun(call$start_and_wait())

  # output does not exist
  fs::file_delete(path_workflow(call$outputs$sample[[1]]$string))
  expect_is(call$outputs$sample[[1]]$validate(), "character")
  expect_error(expect_output(call$wait()))

  # input does not exist
  fs::file_delete(path_workflow(call$inputs$script[[1]]$path))
  expect_error(expect_output(call$start_and_wait()))
})

