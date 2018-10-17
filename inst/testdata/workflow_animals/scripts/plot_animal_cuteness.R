library(tibble)
library(ggplot2)

animal_cuteness <- readr::read_csv(inputs[1])

plot <- ggplot(animal_cuteness, aes(animal, cuteness)) +
  geom_bar(stat = "identity", fill = "red")

ggsave(outputs[1], plot, width = 5, height = 5)
