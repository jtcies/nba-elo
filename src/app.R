library(tidyverse)
library(shiny)
library(here)

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
      dateRangeInput(
        "dates", "Date range",
        start = "1997-10-01",
        end = "2018-10-01",
        format = "mm/dd/yyyy"),
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
    
    elo_plot <- elo %>% 
      filter(
        date > input$dates[[1]], 
        date < input$dates[[2]],
        pretty_name == input$team
      ) %>% 
      ggplot(aes(date, elo, group = team)) +
        geom_line()
    
    elo_plot
  })
  
}

shinyApp(ui = ui, server = server)