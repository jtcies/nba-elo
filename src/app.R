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
      radioButtons(
        "option", "Display by:",
        choices = list("season" = "season", 
                       "date range" = "date_range"),
        selected = "season",
      ),
      input_options(),
      selectInput(
        "team", "Team",
        choices = unique(elo$pretty_name),
        selected = "Philadelphia 76ers"
      )
    ),
    mainPanel(
      plotOutput("elo_plot")
    )
  )
)

server <- function(input, output) {
  
  output$elo_plot <- renderPlot({
    
    filtered_all <- reactive ({
      if (input$option == "date_range") {
        elo %>% 
          filter(
            date >= input$dates[[1]], 
            date <= input$dates[[2]]
          )
      }
      if (input$option == "season"){
        elo %>% 
          filter(
            season == input$season
          )
      }
    })
    
    filtered_team <- filtered_all() %>% 
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
          data = filtered_all(), 
          aes(date, elo, group = team), 
          alpha = 0.1
        ) + 
        scale_x_date(
          breaks = seq.Date(
            from = ymd(paste0(year(input$dates[[1]]), "1001")),
            to = ymd(paste0(year(input$dates[[2]]), "1001")),
            by = "1 year"
          ), 
          date_labels = as.character(unique(filtered_all$season))
        ) +
        labs(x = "season")
    
    elo_plot
  })
  
}

shinyApp(ui = ui, server = server)