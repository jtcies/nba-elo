library(tidyverse)
library(elo)

# functions -------------
for(i in seq_len(nrow(data))) {
# function for creating a running elo
  game <- data[i, ]
  
  # extract elo adding in home/away adjustment
  elo1 <- teams[teams$team == game$home_team, "elo"][[1]] - 100
  elo2 <- teams[teams$team == game$vis_team, "elo"][[1]] + 100
  
  elo_update <- elo.calc(
    score(game["home_pts"], game["visitor_pts"]),
    elo1,
    elo2,
    k = 20 # recommended by 538
  )
  
  new_elo <- elo_update %>% 
    gather() %>% 
    rename(elo = value) %>% 
    mutate(
      team = c(game$home_team, game$vis_team),
      date = game$date
    ) %>% 
    select(event, group, elo)
  
  teams <- teams %>% 
    bind_rows(new_elo) %>% 
    arrange(desc(date))
}
return(teams)
}

# create a base elo table
# start everyone at 1500 in 2001 season
# for expansion teams, start at 1300 in new season, rec by 538
# to carry over for each year, may be easiest to create a df
# for each season with date 10/1 and add carry over elo there