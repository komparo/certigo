library(tibble)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

animal_cuteness <- readr::read_csv(args[1])

plot <- ggplot(animal_cuteness, aes(animal, cuteness)) +
  geom_bar(stat = "identity", fill = "red")

ggsave(args[2], plot)
