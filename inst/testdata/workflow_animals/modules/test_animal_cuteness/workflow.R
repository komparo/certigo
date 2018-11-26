get_call <- function(
  aggregate_animal_cuteness
) {
  rscript_call(
    "test_animal_cuteness",
    design = aggregate_animal_cuteness$design %>%
      select(animal_cuteness) %>%
      mutate(
        script = list(script_file("test_animal_cuteness.R")),
        animal_cuteness_tests = list(derived_file("animal_cuteness_tests.csv"))
      ),
    inputs = exprs(script, animal_cuteness),
    outputs = exprs(animal_cuteness_tests)
  )
}
