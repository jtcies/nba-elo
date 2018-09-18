library(lubridate)

assign_season <- function(date) {
  # determine season based on date of game
  season <- integer(length = length(date))
  season[month(date) >= 10] <- year(date[month(date) >= 10]) + 1
  season[month(date) < 10] <- year(date[month(date) < 10])
  season
}