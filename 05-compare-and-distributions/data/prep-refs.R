## prepare referee data

library(tidyverse)

refs <- read_csv("04-likelihoods/data/basketball0910.csv")

## take the first three fouls for each game
refs_sub <- refs %>%
  group_by(game) %>%
  arrange(desc(time)) %>%
  slice(1:3) %>%
  ungroup()

# determine which team was called

refs_sub <- refs_sub %>%
  mutate(team_called = if_else(foul.home == 1, "H", "V"))

refs_sub_wide <- refs_sub %>%
  select(game, date, visitor, hometeam,foul.num, team_called) %>%
  pivot_wider(id_cols = game:hometeam,
    names_from = foul.num,
               values_from = team_called)


# count pattern for first three fouls 

refs_sub_wide %>%
  count(`1`, `2`, `3`) %>%
  filter(n > 1)

## take a sample of 30 games for simplicity 

set.seed(011721)
lec_04_data <- refs_sub_wide %>%
  sample_n(30)

# rename columns and save 

lec_04_data <- lec_04_data %>%
  rename("foul1" = "1", 
         "foul2" = "2", 
         "foul3" = "3") %>%
  select(-`4`) 


lec_04_data %>%
  write_csv("04-likelihoods/data/04-refs.csv")