library(tidyverse)
library(elo)
library(lubridate)
library(here)

# get the assign season function
source(here::here("src/helper_funs.R"))

# functions -------------
elo_calc_in_season <- function(games, teams){
  
  for(i in seq_len(nrow(games))) {
    # function for creating a running elo
    game <- games[i, ]
    
    # extract most recent elo adding in home/away adjustment
    elo_home <- teams[teams$team == game$home_team, "elo"][[1]] - 100
    elo_vis <- teams[teams$team == game$vis_team, "elo"][[1]] + 100
    
    elo_update <- elo.calc(
      score(game["home_pts"], game["visitor_pts"]),
      elo_home,
      elo_vis,
      k = 20 # recommended by 538
    )
    # reshape elo update
    new_elo <- elo_update %>% 
      gather() %>% 
      rename(elo = value) %>% 
      mutate(
        team = c(game$home_team, game$vis_team),
        date = game$date,
        season = game$season
      ) %>% 
      select(date, team, elo, season)
    
    # give the home/away adjustment back
    new_elo[1, "elo"] <- new_elo[1, "elo"] + 100
    new_elo[2, "elo"] <- new_elo[2, "elo"] - 100
    
    # bind elo update to teams data for running list
    teams <- bind_rows(new_elo, teams)
  }
  return(teams)
}

carry_over <- function(teams) {
# at the start of every new season, carry over 75% of elo
# rec by 538, start new season on 10/01
  new_season <- teams %>% 
    filter(season == season[[1]]) %>% 
    distinct(team, .keep_all = TRUE) %>% 
    mutate(
      elo = (.75 * elo) + (.25 * 1505),
      date = ymd(paste0(season, "1001")),
      season = season + 1 # add one for start of new season
  )
  bind_rows(new_season, teams)
}

elo_calc <- function(games, teams) {
  
  for(j in unique(games$season)) {
    # run the calculation within each season
    season_games <- games[games$season == j, ]
  
    teams <- elo_calc_in_season(season_games, teams)
    # then apply the carryover
    teams <- carry_over(teams)
  }
  return(teams)
}

# import and create new teams table ----------------------

nba <- read_csv(here::here("output/nba_cleaned.csv"))

# create a base elo table
# start everyone at 1500 in 2001 season
# for expansion teams, start at 1300 in new season, rec by 538
teams <- nba %>% 
  distinct(home_team) %>% 
  rename(team = home_team) %>% 
  mutate(
    season = if_else(team == "CHA", 2005, 2001),
    elo = if_else(team == "CHA", 1300, 1500),
    date = ymd(paste0(season, "1001"))
  )

fill_elo <- function(data) {
  # function to fill in missing elo
  data %>% 
    complete(date = full_seq(date, period = 1), team) %>% 
    arrange(date) %>% 
    fill(elo)
}

# run the function and clean up -----------------
# this will take a few minutes to run
running_elo <- elo_calc(nba, teams)

# fill in missing dates

complete_elo <- running_elo %>% 
  split(.$team) %>% 
  map_dfr(fill_elo) %>% 
  mutate(season = assign_season(date))

# write ----------------------

write_csv(complete_elo, here::here("output/running_elo.csv"))