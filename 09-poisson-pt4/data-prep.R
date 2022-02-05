# weekend drinks data prep

library(tidyverse)

drinks <- read_csv("08-poisson-pt3/data/weekendDrinks.csv")

drinks <- drinks %>%
  mutate(first_year = if_else(dorm%in%c("kildahl","mohn","kittlesby"), 1, 0), 
         off_campus = if_else(dorm=="off campus",1,0))
         

drinks %>%
  write_csv("08-poisson-pt3/data/weekend-drinks.csv")
