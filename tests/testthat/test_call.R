context("Testing individual call")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

test_that("Rscript calls", {
  call <- rscript_call(
    "determine_animal_cuteness",
    design = design,
    inputs = exprs(parameters, script),
    outputs = exprs(animal_cuteness)
  )

  expect_rerun(call$start_and_wait())

  # with docker executor and resources
  call <- rscript_call(
    "determine_animal_cuteness",
    design = design %>%
      mutate(executor = list(docker_executor(container = "certigo/animal_cuteness"))),
    inputs = exprs(parameters, script, executor),
    outputs = exprs(animal_cuteness, resources)
  )

  expect_rerun(call$start_and_wait())

  resources <- jsonlite::read_json(path_workflow(design$resources[[1]]$path))
  expect_true(is.list(resources))

  # with default executor
  options(certigo_executor = local_executor())
  call <- rscript_call(
    "determine_animal_cuteness",
    design = design,
    inputs = exprs(parameters, script),
    outputs = exprs(animal_cuteness)
  )
  expect_is(call$calls[[1]]$executor, "LocalExecutor")
  expect_rerun(call$start_and_wait())
})

