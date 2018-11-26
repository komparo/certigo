context("Testing workflow")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

expect_rerun_somewhere(animal_workflow$run())
animal_workflow$reset()
expect_cached_somewhere(animal_workflow$run())
