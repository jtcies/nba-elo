library(tidyverse)
library(shiny)
library(here)
library(lubridate)

source(here::here("src/app_helpers.R"))

scores <- read_csv(here::here("output/nba_cleaned.csv"), guess_max = 5000)
elo <- read_csv(here::here("output/running_elo.csv"), guess_max = 5000)
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

elo_scores <- elo_scores %>% 
  mutate(
    vis_win_prob = make_pct(1 - home_win_prob),
    home_win_prob = make_pct(home_win_prob),
    `OT?` = if_else(is.na(ot), "", "yes"),
    notes = if_else(is.na(notes), "", "notes"),
    playoffs = if_else(playoffs == 1, "yes", "")
  ) %>% 
  select(
    date,
    vis_win_prob,
    visitor,
    visitor_pts,
    home_pts,
    home,
    home_win_prob,
    `OT?`,
    playoffs
  )

ui <- fluidPage(
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "ELO",
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
    ),
    tabPanel(
      "Compare",
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
            column(4, align = "center", htmlOutput("vis_logo")),
            column(4),
            column(4, align = "center", htmlOutput("home_logo"))
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
    elo.prob(home_dat()$elo + 35, vis_dat()$elo - 35)
  })
  
  date_games <- reactive({
    dat <- elo_scores %>% 
      filter(date == input$date) %>% 
      select(-date) %>% 
      rename(`away points` = visitor_pts,
             `home points` = home_pts,
             `away` = visitor,
             `away win prob.` = vis_win_prob,
             `home win prob.` = home_win_prob,
             `playoffs?` = playoffs)
    dat
  })
  
  output$home_elo <- renderText({
    paste0(round(home_dat()$elo), "\n(plus home bonus)")
  })
  
  output$vis_elo <- renderText({
    paste0(round(vis_dat()$elo), "\n(minus away penalty)")
  })
  
  output$home_win_prob <- renderText({make_pct(prob())})
  output$vis_win_prob <- renderText({make_pct(1 - prob())})
  output$games_date <- renderTable({date_games()})
  
  output$home_logo <- renderText({
    name <- unique(scores$home_team[scores$home == input$home_team])
    paste0(
      '<img src="http://stats.nba.com/media/img/teams/logos/',
      name,
      '_logo.svg" style="width:200px;height:200px">'
    )
  })
  
  output$vis_logo <- renderText({
    name <- unique(scores$vis_team[scores$visitor == input$vis_team])
    paste0(
      "<img src='http://stats.nba.com/media/img/teams/logos/",
      name,
      "_logo.svg' style='width:200px;height:200px'>"
    )
  })
}

shinyApp(ui = ui, server = server)