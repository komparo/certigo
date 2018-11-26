context("Testing individual call")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

call <- rscript_call(
  "determine_animal_cuteness",
  design = design,
  inputs = exprs(parameters, script),
  outputs = exprs(animal_cuteness)
)

expect_rerun(call$start_and_wait())

call <- rscript_call(
  "determine_animal_cuteness",
  design = design,
  inputs = exprs(parameters, script),
  outputs = exprs(animal_cuteness, resources)
)

expect_rerun(call$start_and_wait())

resources <- jsonlite::read_json(path_workflow(design$resources[[1]]$path))
expect_true(is.list(resources))
