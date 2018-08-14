library(tidyverse)
library(here)
library(lubridate)

# this script:
# - creates a variable for the correct season
# - creates a variable for a team code which persists; rules:
#     - Char moved to NO in 2002
#     - Char of 2004 is new
#     - Seattle moved to OKC in 2008

# do I also need to figure out playoffs? 

# functions -----------

assign_season <- function(date) {
  season <- integer(length = length(date))
  season[month(date) >= 10] <- year(date[month(date) >= 10]) + 1
  season[month(date) < 10] <- year(date[month(date) < 10])
  season
}

recode_team <- function(data, team_name, new_name) {
  
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
        !!team_name == "Philadelphia 76ers" ~ "PHL",
        !!team_name == "Phoenix Suns" ~ "PHX",
        !!team_name == "Utah Jazz" ~ "UTA",
        !!team_name == "Toronto Raptors" ~ "TOR",
        !!team_name == "Orlando Magic" ~ "ORL",
        !!team_name == "Los Angeles Clippers" ~ "LAC",
        !!team_name == "Washington Wizards" ~ "WSH",
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
        !!team_name == "Seattle Supersonics" ~ "OKC",
        !!team_name == "Brooklyn Nets" ~ "BKN",
        !!team_name == "Charlotte Hornets" & season %in% c(2001:2002) ~ "NOP",
        !!team_name == "Charlotte Hornets" & season > 2014 ~ "CHA",
        !!team_name == "New Orleans Pelicans" ~ "NOP",
        !!team_name == "New Orleans/Oklahoma City Hornets" ~ "NOP",
        !!team_name == "Vancouver Grizzlies" ~ "MEM",
        TRUE ~ NA_character_
      )
    )
}

# import ----------------


nba <- read_csv(here::here("output/nba_scores_2001-2018.csv"))

# transform -------------------

nba$season <- assign_season(nba$date)

nba %>% 
  recode_team(home, "home_team") %>% 
  recode_team(visitor, "vis_team")



