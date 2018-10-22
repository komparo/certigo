library(tibble)
library(ggplot2)

animal_cuteness <- readr::read_csv(inputs["animal_cuteness"])

plot <- ggplot(animal_cuteness, aes(animal, cuteness)) +
  geom_boxplot(aes(fill = animal))

ggsave(outputs["plot"], plot, width = 5, height = 5)
