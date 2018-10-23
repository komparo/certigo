library(ggplot2)

animal_cuteness <- readr::read_csv(inputs[["animal_cuteness"]])
animal_cuteness_tests <- readr::read_csv(inputs[["tests"]])

plot <- ggplot(animal_cuteness_tests) +
  geom_tile(aes(from, to, fill = p_value))

ggsave(outputs[[1]], plot, width = 5, height = 5)
