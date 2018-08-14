library(tidyverse)
library(rvest)

test_url <- "https://www.basketball-reference.com/leagues/NBA_2018_games-october.html"

scrape_br <- function(url) {
# function to scrape the data and organize it into a data frame
  tmp <- url %>% 
    read_html() %>% 
    html_nodes(".right , .left , .center") %>% 
    html_text()
  
  tmp <- gsub(",", "", tmp) # remove commas
  
  tmp <- paste0(tmp, ",") # add a commoa to the end of each line
  # start a new row after each 10th element
  new_rows <- seq(0, length(tmp), by = 10)
  tmp[new_rows] <- paste0(tmp[new_rows], "\n")
  # collapse to a chacter string then read as csv
  dat <- paste0(tmp, collapse = "") %>% 
    read.csv(text = .) %>% 
    filter(!is.na(PTS))
  
  dat
}

months <- c("october", "november", "december", "january", "february", 
            "march", "april", "may", "june")

years <- seq(2008, 2018, by = 1)

season_months <- expand.grid(years, months)

br_urls <- paste0(
  "https://www.basketball-reference.com/leagues/NBA_", 
  season_months$Var1, 
  "_games-",
  season_months$Var2,
  ".html"
)

br_dat <- map(br_urls, ~ifelse(RCurl::url.exists(.), ., NA)) %>% 
  .[!is.na(.)] %>% 
  map_dfr(., scrape_br)