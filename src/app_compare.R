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
      textOutput("vis_win_prob"),
      h2("real games on this day"),
      tableOutput("games_date")
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
  
  vis_dat <- reactive({ 
    elo %>% 
      filter(
        pretty_name == input$vis_team,
        date == input$date
      )
  })

  
  prob <- reactive({
    elo.prob(home_dat()$elo + 100, vis_dat()$elo - 100)
  })
  
  
  date_games <- reactive({
    
    dat <- scores %>% 
      filter(date == test_date) %>% 
      left_join(
        elo %>% 
          rename(vis_elo = elo) %>% 
          filter(date %in% c(test_date, test_date - 1)),
        by = c("vis_team" = "team")
      ) %>% 
      left_join(
        elo %>% 
          rename(home_elo = elo) %>% 
          filter(date %in% c(test_date, test_date -1)),
        by = c("home_team" = "team")
      ) %>% 
      arrange(date) %>% 
      mutate(
        prev_home_elo = lag(home_elo),
        prev_vis_elo = lag(vis_elo)
      ) %>% 
      filter(date == test_date) %>% 
      select(visitor, visitor_pts, home, home_pts,
             prev_vis_elo, prev_home_elo,
             vis_elo_after = vis_elo, home_elo_after = home_elo)
    
    dat
  })
  
  output$home_elo <- renderText({
    paste0(round(home_dat()$elo), " (plus home bonus)")
  })
  
  output$vis_elo <- renderText({
    paste0(round(vis_dat()$elo), " (minus away penalty)")
  })
  
  output$home_win_prob <- renderText({make_pct(prob())})
  output$vis_win_prob <- renderText({make_pct(1 - prob())})
  
  output$games_date <- renderTable({date_games()})
}


shinyApp(ui, server)
