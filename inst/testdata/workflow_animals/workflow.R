library(dplyr)
library(tibble)
library(purrr)
library(stringr)

library(certigo)

design <- tibble(
  animal = c("dog", "cat", "horse", "tortoise", "fly", "bird"),
  cuteness_mean = c(1, 0.9, 0.8, 0.6, 0.1, 0.4),
  n_animals = c(10, 20, 30, 10, 20, 30)
) %>%
  mutate(
    parameters = dynutils::mapdf(., parameters),
    script = list(script_file("scripts/determine_animal_cuteness.R")),
    executor = list(docker_executor(container = "rocker/tidyverse")),
    animal_cuteness = str_glue("derived/animal_cuteness/{animal}.csv") %>% map(derived_file)
  )

determine_animal_cuteness <- rscript_call(
  "determine_animal_cuteness",
  design = design,
  inputs = exprs(parameters, script, executor),
  outputs = exprs(animal_cuteness)
)

aggregate_animal_cuteness <- rscript_call(
  "aggregate_animal_cuteness",
  design = list(
    animal_cuteness_individual = object_set(determine_animal_cuteness$design$animal_cuteness),
    script = script_file("scripts/aggregate_animal_cuteness.R"),
    animal_cuteness = derived_file("derived/animal_cuteness.csv")
  ),
  inputs = exprs(script, animal_cuteness_individual),
  outputs = exprs(animal_cuteness)
)

plot_animal_cuteness <- rscript_call(
  "plot_animal_cuteness",
  design = aggregate_animal_cuteness$design %>%
    select(animal_cuteness) %>%
    mutate(
      script = list(script_file("scripts/plot_animal_cuteness.R")),
      plot = list(derived_file("results/animal_cuteness.pdf"))
    ),
  inputs = exprs(script, animal_cuteness),
  outputs = exprs(plot)
)

test_animal_cuteness <- load_call(
  "modules/test_animal_cuteness/workflow.R",
  aggregate_animal_cuteness = aggregate_animal_cuteness,
  derived_file_directory = "derived/"
)

plot_animal_cuteness_tests <- rscript_call(
  "plot_animal_cuteness_tests",
  design = bind_cols(
    aggregate_animal_cuteness$outputs,
    test_animal_cuteness$outputs
  ) %>% mutate(
    script = list(script_file("scripts/plot_animal_cuteness_tests.R")),
    plot = list(derived_file("results/animal_cuteness_tests.pdf"))
  ),
  inputs = exprs(script, animal_cuteness, animal_cuteness_tests),
  outputs = exprs(plot)
)

overview <- rmd_call(
  "overview",
  design = list(
    script = script_file("scripts/overview.Rmd"),
    executor = docker_executor("rocker/tidyverse"),
    rendered = derived_file("results/overview.html")
  ),
  inputs = exprs(script, executor),
  outputs = exprs(rendered)
)

always_error <- rscript_call(
  "always_error",
  design =  aggregate_animal_cuteness$outputs %>%
    mutate(
      script = list(script_file("scripts/always_error.R")),
      plot = list(derived_file("results/error.pdf"))
    ),
  inputs = exprs(animal_cuteness, script),
  outputs = exprs(plot)
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
  plotting,
  overview
)

animal_workflow$plot()

