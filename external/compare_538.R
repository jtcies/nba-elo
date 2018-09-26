library(tidyverse)
library(here)
library(jtcr)

# get cor and plot my and 538's elo

# known differences:
# 1. mine doesn't account for win margin

f38 <- read_csv(here::here("data/nba_elo_538.csv"))

elo <- read_csv(here::here("output/running_elo.csv"), guess_max = 5000)

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
      team == "CHH" & season %in% c(1997:2002) ~ "NOP",
      team == "CHH" & season > 2014 ~ "CHA",
      team == "CHO" ~ "CHA",
      team == "NJN" ~ "BKN",
      team == "NOK" ~ "NOP",
      team == "WSB" ~ "WAS", 
      TRUE ~ team
    )
  ) %>% 
  filter(season %in% elo$season)

# combine
both <- f38_tidy %>% 
  select(-season) %>% 
  left_join(elo, by = c("date", "team")) %>% 
  filter(season > 1997)

rsq <- round(cor(both$elo, both$elo_538), 2)

# plot
both %>% 
  ggplot(aes(elo, elo_538)) +
    geom_point(color = jtc_blues[[3]], alpha = 0.5) +
    geom_smooth(method = "lm", color = jtc_oranges[[2]]) +
    labs(
      title = "Calculated Elo vs 538 benchmark",
      x = "calculated Elo",
      y = "538 Elo",
      subtitle = paste0("r^2 = ", rsq)
    ) +
    theme_jtc() 
  

ggsave(here::here("output/figs/538_compare.png"))
ggsave(here::here("../../jtcies_site2/static/img/nba-elo/538_compare.png"))

lm(elo_538 ~ elo, data = both)

# correlation by year

both %>% 
  split(.$season) %>% 
  map(~cor(.$elo, .$elo_538))
