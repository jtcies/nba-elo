library(dplyr)
library(ggplot2)
library(shiny)
library(here)
library(lubridate)
library(jtcr)
library(huxtable)
library(readr)
library(tidyr)
library(elo)

source(here::here("global.R"))

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
    `OT?` = if_else(is.na(ot), "", "yes"),
    notes = if_else(is.na(notes), "", "notes"),
    playoffs = if_else(playoffs == 1, "yes", "")
  ) 

ui <- fluidPage(
  
    includeCSS("www/jtc_style_min.css"),
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "ELO",
      sidebarLayout(
        sidebarPanel(
          p("Choose a team and date range to see ELO change over time."),
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
          p("Choose any two teams and any date to see how they match up."),
          p("Below you can see actual NBA games on that day and their result."),
          selectInput(
            "vis_team", "Away Team",
            choices = unique(elo$pretty_name),
            selected = "Golden State Warriors"
          ),
          selectInput(
            "home_team", "Home Team",
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
            column(4, align = "center", htmlOutput("vis_logo")),
            column(4, align = "center", h3("at"),
                   style = "margin-top: 100px"),
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
          htmlOutput("games_date"),
          p("Note: blue represents an expected result, 
            orange represents an upset")
        )
      )
    ),
    tabPanel(
      "About",
      br(),
      p("All data from Basketball Reference (https://www.basketball-reference.com)"),
      p("Team logos retrieved from https://stats.nba.com")
    )
  )
)

server <- function(input, output) {

# set up all of the outputs
  elo_dates <- reactive({
    elo %>% 
      filter(
        date >= input$date_range[[1]],
        date <= input$date_range[[2]]
      )
  })
    
  elo_team <- reactive({
    elo_dates() %>% 
      filter(pretty_name == input$team)
  })

  output$elo_plot <- renderPlot({
    
      elo_plot <- ggplot() +
        geom_line(
          data = elo_team(), 
          aes(date, elo, group = team), 
          size = 1.5,
          color = jtc_oranges[[2]]
        ) +
        geom_line(
          data = elo_dates(), 
          aes(date, elo, group = team), 
          alpha = 0.1,
          color = jtc_blues[[3]]
        ) + 
        geom_hline(
          aes(yintercept = 1500),
          linetype = 3
        ) +
        labs(
          title = "Team ELO over time",
          x = "date") +
        theme_jtc()
    
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
      mutate(
        vis_prob = make_pct(1 - home_win_prob),
        home_prob = make_pct(home_win_prob)
      ) %>% 
      as_hux() %>% 
      expected_highlight() %>% 
      upset_highlight() %>% 
      home_win_bold() %>% 
      vis_win_bold() %>% 
      select(
        `away win prob.` = vis_prob,
        `away` = visitor,
        `away points` = visitor_pts,
        `home points` = home_pts,
        home,
        `home win prob.` = home_prob,
        `OT?`,
        `playoffs?` = playoffs
      ) %>% 
      add_colnames() %>% 
      to_html()
  
    dat
  })

# create the final output
  output$home_elo <- renderText({
    paste0(round(home_dat()$elo), "\n(plus home bonus)")
  })
  
  output$vis_elo <- renderText({
    paste0(round(vis_dat()$elo), "\n(minus away penalty)")
  })
  
  output$home_win_prob <- renderText({make_pct(prob())})
  output$vis_win_prob <- renderText({make_pct(1 - prob())})
  output$games_date <- renderText({date_games()})
  
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