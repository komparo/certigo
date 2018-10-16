library(ggplot2)

animal_cuteness <- readr::read_tsv(inputs[1])
animal_cuteness_tests <- readr::read_csv(inputs[2])

plot <- ggplot(animal_cuteness_tests) +
  geom_tile(aes(from, to, fill = p_value))

ggsave(outputs[1], plot)
