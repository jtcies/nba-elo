# add in the pre and post game elo for every game

library(tidyverse)
library(here)

elo <- read_csv(here::here("output/running_elo.csv"), guess_max = 5000)
scores <- read_csv(here::here("output/nba_cleaned.csv"), guess_max = 5000)

elo_scores <- scores %>% 
  left_join(
    elo %>% 
      rename(home_elo_after = elo) %>% 
      select(-season),
    by = c("date", "home_team" = "team")
  ) %>% 
  left_join(
    elo %>% 
      rename(vis_elo_after = elo) %>% 
      select(-season),
    by = c("date", "vis_team" = "team")
  )

before_elo_home <- elo_scores %>% 
  select(date, home_team, home_elo_after, game_id, season) %>% 
  group_by(home_team) %>% 
  arrange(date) %>% 
  mutate(home_elo_before = lag(home_elo_after)) %>% 
  ungroup() %>% 
  select(-date, -home_team, -home_elo_after)

before_elo_vis <- elo_scores %>% 
  select(date, vis_team, vis_elo_after, game_id, season) %>% 
  group_by(vis_team) %>% 
  arrange(date) %>% 
  mutate(vis_elo_before = lag(vis_elo_after)) %>% 
  ungroup() %>% 
  select(-date, -vis_team, -vis_elo_after)

elo_scores <- elo_scores %>% 
  left_join(before_elo_home, by = c("season", "game_id")) %>% 
  left_join(before_elo_vis, by = c("season", "game_id"))

write_csv(elo_scores, here::here("output/nba_scores_w_elo.csv"))  

