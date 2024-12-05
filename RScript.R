####How does life expectancy change across the years within England?

#Install relevant packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("here")
install.packages("gganimate")

#Load packages
library(tidyverse)
library(here)

#Load data
load.csv(trends_in_life_expectancy)


#Keep only rows for England, Northern Island, and Wales
data2 <- data %>% filter(Area.name %in% c("England", "Northern Ireland", "Wales"))
