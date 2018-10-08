library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
path_animal_coolness <- args[1]
path_animal_coolness_tests <- args[2]
path_output <- args[3]

animal_coolness <- readr::read_tsv(path_animal_coolness)
animal_coolness_tests <- readr::read_tsv(path_animal_coolness_tests)
