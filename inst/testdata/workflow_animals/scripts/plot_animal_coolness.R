library(tibble)
library(ggplot2)

animal_coolness <- readr::read_tsv(inputs[1])

plot <- ggplot(animal_coolness, aes(animal, coolness)) +
  geom_bar(stat = "identity", fill = "red")

ggsave(outputs[1], plot)
