library(tidyverse)
library(here)

# get cor and plot my and 538's elo

# known differences:
# 1. mine doesn't account for win margin

f38 <- read_csv(here::here("data/nba_elo_538.csv"))

elo <- read_csv(here::here("output/running_elo.csv"))

# clean up f38
# take post game elo

f38_tidy1 <- f38 %>% 
  select(date, season, team = team1, elo_538 = elo1_post)

f38_tidy2 <- f38 %>% 
  select(date, season, team = team2, elo_538 = elo2_post)

f38_tidy <- bind_rows(f38_tidy1, f38_tidy2) %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    team = case_when(
      team == "VAN" ~ "MEM",
      team == "SEA" ~ "OKC",
      team == "BRK" ~ "BKN",
      team == "CHH" & season %in% c(2001:2002) ~ "NOP",
      team == "CHH" & season > 2014 ~ "CHA",
      team == "CHO" ~ "CHA",
      team == "NJN" ~ "BKN",
      team == "NOK" ~ "NOP",
      TRUE ~ team
    )
  ) %>% 
  filter(season %in% elo$season)

# combine
both <- f38_tidy %>% 
  select(-season) %>% 
  left_join(elo, by = c("date", "team"))

# plot
ggplot(both, aes(elo, elo_538, color = season)) +
  geom_point()

# correlation by year

both %>% 
  split(.$season) %>% 
  map(~cor(.$elo, .$elo_538))
