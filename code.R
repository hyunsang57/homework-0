library(tidyverse)
library(dslabs)
head(heights)

heights %>% 
  ggplot(aes(height, ..density.., fill = sex)) +
  geom_density(alpha = 0.3)