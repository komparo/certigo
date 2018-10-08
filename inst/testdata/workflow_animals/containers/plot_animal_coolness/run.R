library(tibble)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

animal_coolness <- readr::read_tsv(args[1])

plot <- ggplot(animal_coolness, aes(animal, coolness)) +
  geom_bar(stat = "identity", fill = "red")

ggsave(args[2], plot)
