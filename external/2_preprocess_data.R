library(tidyverse)
library(here)
library(lubridate)

# get the assign season function
source(here::here("external/helper_funs.R"))

# this script:
# - creates a variable for the correct season
# - creates a variable for a team code which persists; rules:
#     - Char moved to NO in 2002
#     - Char of 2004 is new
#     - Seattle moved to OKC in 2008

# functions -----------

recode_team <- function(data, team_name, new_name) {
  # reocde all team names
  team_name <- enquo(team_name)
  recode <- quo_name(enquo(new_name))
  
  data %>%
    mutate(
      !!recode := case_when(
        !!team_name == "San Antonio Spurs" ~ "SAS",
        !!team_name == "Boston Celtics" ~ "BOS",
        !!team_name == "Miami Heat" ~ "MIA",
        !!team_name == "Los Angeles Lakers" ~ "LAL",
        !!team_name == "Cleveland Cavaliers" ~ "CLE",
        !!team_name == "Dallas Mavericks" ~ "DAL",
        !!team_name == "Golden State Warriors" ~ "GSW",
        !!team_name == "Indiana Pacers" ~ "IND",
        !!team_name == "Houston Rockets" ~ "HOU",
        !!team_name == "Atlanta Hawks" ~ "ATL",
        !!team_name == "Chicago Bulls" ~ "CHI",
        !!team_name == "Philadelphia 76ers" ~ "PHI",
        !!team_name == "Phoenix Suns" ~ "PHO",
        !!team_name == "Utah Jazz" ~ "UTA",
        !!team_name == "Toronto Raptors" ~ "TOR",
        !!team_name == "Orlando Magic" ~ "ORL",
        !!team_name == "Los Angeles Clippers" ~ "LAC",
        !!team_name == "Washington Wizards" ~ "WAS",
        !!team_name == "Denver Nuggets" ~ "DEN",
        !!team_name == "Milwaukee Bucks" ~ "MIL",
        !!team_name == "Portland Trail Blazers" ~ "POR",
        !!team_name == "Sacramento Kings" ~ "SAC",
        !!team_name == "Minnesota Timberwolves" ~ "MIN", 
        !!team_name == "New York Knicks" ~ "NYK",
        !!team_name == "Memphis Grizzlies" ~ "MEM",
        !!team_name == "New Jersey Nets" ~ "BKN",
        !!team_name == "Oklahoma City Thunder" ~ "OKC",
        !!team_name == "Charlotte Bobcats" ~ "CHA",
        !!team_name == "New Orleans Hornets" ~ "NOP",
        !!team_name == "Seattle SuperSonics" ~ "OKC",
        !!team_name == "Brooklyn Nets" ~ "BKN",
        !!team_name == "Detroit Pistons" ~ "DET",
        !!team_name == "Charlotte Hornets" & season %in% 1997:2002 ~ "NOP",
        !!team_name == "Charlotte Hornets" & season > 2014 ~ "CHA",
        !!team_name == "New Orleans Pelicans" ~ "NOP",
        !!team_name == "New Orleans/Oklahoma City Hornets" ~ "NOP",
        !!team_name == "Vancouver Grizzlies" ~ "MEM",
        !!team_name == "Washington Bullets" ~ "WAS",
        TRUE ~ NA_character_
      )
    )
}

# import ----------------

nba <- read_csv(here::here("output/nba_scores_1997-2018.csv"))

# transform -------------------

# season
nba$season <- assign_season(nba$date)

# name recode
nba <- nba %>% 
  recode_team(home, "home_team") %>% 
  recode_team(visitor, "vis_team")

# game number and playoffs

nba <- nba %>% 
  arrange(date) %>% 
  group_by(season) %>% 
  mutate(game_id = row_number()) %>% 
  ungroup()

nba_long <- nba %>% 
  select(season, game_id, home_team, vis_team) %>% 
  gather(game, team, 3:4) %>% 
  arrange(season, game_id) %>% 
  group_by(season, team) %>% 
  mutate(
    game_number = row_number()
  ) %>% 
  ungroup() %>% 
  select(season, game_id, game, game_number) %>% 
  spread(game, game_number) %>% 
  rename(
    home_game_number = home_team, 
    vis_game_number = vis_team
  )

nba <- nba %>% 
  left_join(nba_long, by = c("season", "game_id")) %>% 
  mutate(
    playoffs = case_when(
      season == 2012 & home_game_number > 66 ~ 1,
      home_game_number > 82 ~ 1,
      TRUE ~ 0
    )
  )

write_csv(nba, here::here("output/nba_cleaned.csv"))