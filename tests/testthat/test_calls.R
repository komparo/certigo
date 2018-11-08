context("Testing individual calls")

source(system.file("testdata/workflow_animals/setup.R", package = "certigo"))

##  ............................................................................
##  Test individual calls                                                   ####
# determine_animal_cuteness$start_and_wait()
expect_rerun(determine_animal_cuteness$start_and_wait())

# cached
expect_cached(determine_animal_cuteness$start_and_wait())

# deleted output -> rerun
determine_animal_cuteness$outputs$animal_cuteness[[1]]$delete()
expect_rerun(determine_animal_cuteness$start_and_wait())

# run with multiple inputs
expect_rerun(aggregate_animal_cuteness$start_and_wait())

# test docker execution
expect_rerun(plot_animal_cuteness$start_and_wait())
expect_true(file_exists(plot_animal_cuteness$outputs$plot[[1]]$string))

# no input -> error + deleted output
aggregate_animal_cuteness$outputs$animal_cuteness[[1]]$delete()
expect_error(capture_output(plot_animal_cuteness$start_and_wait()))
expect_false(file_exists(plot_animal_cuteness$outputs$plot[[1]]$string))

# input is again present -> rerun
expect_rerun(aggregate_animal_cuteness$start_and_wait())
expect_rerun(plot_animal_cuteness$start_and_wait())

# test some other calls
expect_rerun(test_animal_cuteness$start_and_wait())

expect_rerun(plot_animal_cuteness_tests$start_and_wait())

expect_error(capture_output(always_error$start_and_wait()))

expect_rerun(overview$start_and_wait())
expect_true(fs::file_exists("results/overview.html"))

# cleanup
setwd(paste0(system.file(package = "certigo"), "/.."))
