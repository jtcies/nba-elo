library(tidyverse)
library(rvest)
library(lubridate)
library(RCurl)
library(here)

scrape_br <- function(url) {
# function to scrape the data and organize it into a data frame
  # table from older years missing start time
  old_format_years <- "1997|1998|1999|2000"
  old_format <- if_else(grepl(old_format_years, url), TRUE, FALSE) 
  table_names <- c("date", "start_time", "visitor", "visitor_pts",
                   "home", "home_pts", "link", "ot", "attendance",
                   "notes")
  if(old_format) num_col <- 9 else num_col <- 10
  # get the table
  tmp <- url %>% 
    read_html() %>% 
    html_nodes(".right , .left , .center") %>% 
    html_text()
  # prep to read as csv
  tmp <- gsub(",", "", tmp) # remove commas for reading as csv
  # add a comma to the end of each field
  tmp <- paste0(tmp, ",")
  # start a new row after each 9/10 element depending on season
  new_rows <- seq(0, length(tmp), by = num_col)
  tmp[new_rows] <- paste0(tmp[new_rows], "\n")
  # collapse to a chacter string then read as csv
  # convert all cols to character, read.csv was reading some in correctly
  dat <- paste0(tmp, collapse = "") %>% 
    read.csv(text = .,
             colClasses = rep("character", num_col)) %>% 
    filter(PTS != "") # remove rows w/o data
  # clean up column names
  if(old_format) {
    names(dat) <- table_names[table_names != "start_time"]
  } else {
    names(dat) <- table_names
  }
  dat[!is.na(names(dat))]
}

# create all combinations of months and years
months <- c("october", "november", "december", "january", "february", 
            "march", "april", "may", "june")

years <- seq(1997, 2018, by = 1)

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
  map(., scrape_br) %>% 
  bind_rows()

# clean up
# select the valid columns
br_dat <- br_dat[1:10]
# convert date to date format and sort
br_dat <- br_dat %>% 
  mutate(date = mdy(date)) %>% 
  arrange(date)

# write the data
write_csv(br_dat, here::here("output/nba_scores_1997-2018.csv"))