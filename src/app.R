library(tidyverse)
library(shiny)
library(here)
library(lubridate)

source(here::here("src/app_helpers.R"))

scores <- read_csv(here::here("output/nba_cleaned.csv"), guess_max = 5000)
elo <- read_csv(here::here("output/running_elo.csv"), guess_max = 5000)

# remove 1997 baseline season
# and add in official name
elo <- elo %>% 
  filter(season > 1997) %>% 
  left_join(
    scores %>% 
      distinct(home, home_team, season),
    by = c("team" = "home_team", "season")
  ) %>% 
  rename(pretty_name = home) %>% 
  group_by(team) %>% 
  fill(pretty_name)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "team", "Team",
        choices = unique(elo$pretty_name),
        selected = "Philadelphia 76ers"
      ),
      dateRangeInput(
        "date_range", "Date Range",
        start = "1997-10-01",
        end = "2018-10-01",
        format = "mm/dd/yyyy"
      )
    ),
    mainPanel(
      plotOutput("elo_plot")
    )
  )
)

server <- function(input, output) {

  output$elo_plot <- renderPlot({
    
    
    filtered_team <- elo %>% 
      filter(
        pretty_name == input$team
      )
    
      elo_plot <- ggplot() +
        geom_line(
          data = filtered_team, 
          aes(date, elo, group = team), 
          size = 1.5
        ) +
        geom_line(
          data = elo, 
          aes(date, elo, group = team), 
          alpha = 0.1
        ) + 
        labs(x = "date")
    
    elo_plot
  })
  
}

shinyApp(ui = ui, server = server)