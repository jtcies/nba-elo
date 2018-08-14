library(tidyverse)
library(rvest)
library(lubridate)
library(RCurl)
library(here)

scrape_br <- function(url) {
# function to scrape the data and organize it into a data frame
  # get the table
  tmp <- url %>% 
    read_html() %>% 
    html_nodes(".right , .left , .center") %>% 
    html_text()
  # prep to read as csv
  tmp <- gsub(",", "", tmp) # remove commas for reading as csv
  # add a comma to the end of each field
  tmp <- paste0(tmp, ",")
  # start a new row after each 10th element
  new_rows <- seq(0, length(tmp), by = 10)
  tmp[new_rows] <- paste0(tmp[new_rows], "\n")
  # collapse to a chacter string then read as csv
  # convert all cols to character, read.csv was reading some in correctly
  dat <- paste0(tmp, collapse = "") %>% 
    read.csv(text = .,
             colClasses = rep("character", 10)) %>% 
    filter(PTS != "") # remove rows w/o data
  # clean up column names
  names(dat) <- c("date", "start_time", "visitor", "visitor_pts",
                  "home", "home_pts", "link", "ot", "attendance",
                  "notes")
  
  dat
}

# create all combinations of months and years
months <- c("october", "november", "december", "january", "february", 
            "march", "april", "may", "june")

years <- seq(2001, 2018, by = 1)

season_months <- expand.grid(years, months)

# create a list of urls for scraping data
br_urls <- paste0(
  "https://www.basketball-reference.com/leagues/NBA_", 
  season_months$Var1, 
  "_games-",
  season_months$Var2,
  ".html"
)

# get only valid urls and scrape
br_dat <- map(br_urls, ~ifelse(RCurl::url.exists(.), ., NA)) %>% 
  .[!is.na(.)] %>% 
  map_dfr(., scrape_br)

# clean up
# select the valid columns
br_dat <- br_dat[1:10]
# convert date to date
br_dat$date <- mdy(br_dat$date)

# write the data
write_csv(br_dat, here::here("output/nba_scores_2001-2018.csv"))