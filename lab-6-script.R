
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")
fish

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

fish_long

# do stuff ----------------------------------------------------------------

t.test(formula= species ~ location, data= fish_long)

ggplot(data = fish_long) +
  geom_histogram(mapping = aes(x = species), binwidth = 4,
                 boundary = 0, closed = "left", 
                 fill = "#C5351B", color = "black") +
  labs(x = "Species", y = "Frequency (number of species)") +
  scale_y_continuous(breaks = seq(0, 8, 2), limits = c(0, 8), 
                     expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 36, 4), limits = c(0, 40)) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1))
  )
