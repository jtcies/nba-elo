# add in the pre and post game elo for every game

library(tidyverse)
library(here)
library(elo)

elo <- read_csv(here::here("output/running_elo.csv"), guess_max = 5000)
scores <- read_csv(here::here("output/nba_cleaned.csv"), guess_max = 5000)

elo_scores <- scores %>% 
  left_join(
    elo %>% 
      rename(home_elo_before = elo) %>% 
      select(-season),
    by = c("date", "home_team" = "team")
  ) %>% 
  left_join(
    elo %>% 
      rename(vis_elo_before = elo) %>% 
      select(-season),
    by = c("date", "vis_team" = "team")
  )

# calculate win probs based on prev elo

elo_scores <- elo_scores %>% 
  mutate(
    home_win_prob = elo.prob(home_elo_before + 35, vis_elo_before - 35)
  )

write_csv(elo_scores, here::here("output/nba_scores_w_elo.csv"))  

