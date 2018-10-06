library(tibble)
library(ggplot2)
`%|%` <- rlang::`%|%`

args <- commandArgs(trailingOnly = TRUE)
path_input <- args[1]
path_plot <- args[2]

animal_coolness <- readr::read_tsv(path_input)

plot <- ggplot(animal_coolness, aes(animal, coolness)) +
  geom_bar(stat = "identity", fill = "red")

ggsave(path_plot, plot)