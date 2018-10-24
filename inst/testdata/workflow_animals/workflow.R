library(dplyr)
library(tibble)
library(purrr)
library(stringr)

library(certigo)

design <- tibble(
  animal = c("dog", "cat", "horse", "tortoise", "fly", "bird"),
  cuteness_mean = c(1, 0.9, 0.8, 0.6, 0.1, 0.4),
  n_animals = c(10, 20, 30, 10, 20, 30)
)

determine_animal_cuteness <- rscript_call(
  "determine_animal_cuteness",
  design = design,
  inputs = design %>% transmute(
    parameters = design %>% dynutils::mapdf(parameters),
    script = list(script_file("scripts/determine_animal_cuteness.R")),
    executor = list(docker_executor(container = "rocker/tidyverse"))
  ),
  outputs = design %>%
    transmute(
      animal_cuteness = str_glue("derived/animal_cuteness/{animal}.csv") %>% map(derived_file)
    )
)

aggregate_animal_cuteness <- rscript_call(
  "aggregate_animal_cuteness",
  inputs = tibble(
    script = list(script_file("scripts/aggregate_animal_cuteness.R")),
    animal_cutenesses = list(object_set(determine_animal_cuteness$outputs$animal_cuteness))
  ),
  outputs = tibble(animal_cuteness = list(derived_file("derived/animal_cuteness.csv")))
)

plot_animal_cuteness <- rscript_call(
  "plot_animal_cuteness",
  inputs = aggregate_animal_cuteness$outputs %>%
    mutate(
      script = list(script_file("scripts/plot_animal_cuteness.R"))
    ),
  outputs = list(
    plot = derived_file("results/animal_cuteness.pdf")
  )
)

test_animal_cuteness <- rscript_call(
  "test_animal_cuteness",
  inputs = list(
    script = script_file("scripts/test_animal_cuteness.R"),
    animal_cuteness = derived_file("derived/animal_cuteness.csv")
  ),
  outputs = list(tests = derived_file("derived/animal_cuteness_tests.csv"))
)


plot_animal_cuteness_tests <- rscript_call(
  "plot_animal_cuteness_tests",
  inputs = bind_cols(
    aggregate_animal_cuteness$outputs,
    test_animal_cuteness$outputs
  ) %>% mutate(
    script = list(script_file("scripts/plot_animal_cuteness_tests.R"))
  ),
  outputs = list(plot = derived_file("results/animal_cuteness_tests.pdf"))
)

always_error <- rscript_call(
  "always_error",
  inputs = bind_cols(
    aggregate_animal_cuteness$outputs
  ) %>% mutate(
    script = list(script_file("scripts/always_error.R"))
  ),
  outputs = list(plot = derived_file("results/error.pdf"))
)


plotting <- call_collection(
  "plotting",
  plot_animal_cuteness,
  plot_animal_cuteness_tests
)

animal_workflow <- workflow(
  determine_animal_cuteness,
  aggregate_animal_cuteness,
  test_animal_cuteness,
  plotting
)

animal_workflow$plot()
