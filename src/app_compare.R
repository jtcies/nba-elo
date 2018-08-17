library(tidyverse)
library(shiny)
library(elo)

source(here::here("src/app_helpers.R"))

elo <- read_csv(here::here("output/running_elo.csv"), guess_max = 5000)
scores <- read_csv(here::here("output/nba_cleaned.csv"), guess_max = 5000)
elo_scores <- read_csv(here::here("output/nba_scores_w_elo.csv"), 
                       guess_max = 5000)

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

# also for the app, elo should be beginning of the day rather than end
# of the day which is how the data is current organized. 

elo_scores <- elo_scores %>% 
  mutate(
    vis_win_prob = make_pct(1 - home_win_prob),
    home_win_prob = make_pct(home_win_prob),
    `OT?` = if_else(is.na(ot), "", "yes"),
    notes = if_else(is.na(notes), "", "notes")
  ) %>% 
  select(
    date,
    visitor,
    vis_win_prob,
    visitor_pts,
    home_pts,
    home_win_prob,
    home,
    `OT?`,
    notes
  )

ui <- ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "vis_team", "Away team",
        choices = unique(elo$pretty_name),
        selected = "Golden State Warriors"
      ),
      selectInput(
        "home_team", "Home team",
        choices = unique(elo$pretty_name),
        selected = "Cleveland Cavaliers"
      ),
      dateInput(
        "date", "Date:",
        value = "06-08-2018",
        format = "mm/dd/yyyy",
        min = "1997-10-01",
        max = "2018-10-01"
      )
    ),
    mainPanel(
      fluidRow(
        column(4, align = "center", h3("away team")),
        column(4),
        column(4, align = "center", h3("home team"))
      ),
      fluidRow(
        column(4, align = "center", textOutput("vis_elo")),
        column(4, align = "center", p("ELO")),
        column(4, align = "center", textOutput("home_elo"))
      ),
      fluidRow(
        column(4, align = "center", textOutput("vis_win_prob")),
        column(4, align = "center", p("win probability")),
        column(4, align = "center", textOutput("home_win_prob"))
      ),
      br(),
      br(),
      h2("NBA games on this day"),
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
    dat <- elo_scores %>% 
      filter(date == input$date) %>% 
      select(-date)
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
