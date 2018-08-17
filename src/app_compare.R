library(tidyverse)
library(shiny)
library(elo)

source(here::here("src/app_helpers.R"))

elo <- read_csv(here::here("output/running_elo.csv"), guess_max = 5000)
scores <- read_csv(here::here("output/nba_cleaned.csv"), guess_max = 5000)

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

ui <- ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "home_team", "Home team",
        choices = unique(elo$pretty_name),
        selected = "Cleveland Cavaliers"
      ),
      selectInput(
        "vis_team", "Away team",
        choices = unique(elo$pretty_name),
        selected = "Golden State Warriors"
      ),
      dateInput(
        "date", "Date:",
        value = "06-08-2018",
        format = "mm/dd/yyyy"
      )
    ),
    mainPanel(
      h3("home team"),
      textOutput("home_elo"),
      textOutput("home_win_prob"),
      h3("away team"),
      textOutput("vis_elo"),
      textOutput("vis_win_prob")
    )
  )
)


server <- function(input, output, session) {
  
  home_dat <- reactive({
    elo %>% 
    filter(
      pretty_name == input$home_team,
      date == input$date
    )
  })
  
  vis_dat <- reactive({ elo %>% 
    filter(
      pretty_name == input$vis_team,
      date == input$date
    )
  })
  
  prob <- reactive({
    elo.prob(home_dat()$elo + 100, vis_dat()$elo - 100)
  })
  
  output$home_elo <- renderText({
    paste0(round(home_dat()$elo), " (plus home bonus)")
  })
  
  output$vis_elo <- renderText({
    paste0(round(vis_dat()$elo), " (minus away penalty)")
  })
  
  output$home_win_prob <- renderText({make_pct(prob())})
  output$vis_win_prob <- renderText({make_pct(1 - prob())})
}

shinyApp(ui, server)
