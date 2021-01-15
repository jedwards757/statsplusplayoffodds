library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(formattable)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(ggthemes)

odds_over_time = read.csv('odds_over_time.csv') %>%
  mutate(Week = Week - 13)
draft_odds = read.csv('curr_odds.csv') %>%
  mutate(top_pick_odds= percent(top_pick_odds),
         second_pick_odds = percent(second_pick_odds),
         third_pick_odds = percent(third_pick_odds),
         fourth_pick_odds = percent(fourth_pick_odds),
         fifth_pick_odds = percent(fifth_pick_odds),
         top_five_odds = percent(top_five_odds),
         top_ten_odds = percent(top_ten_odds),
         top_fifteen_odds = percent(top_fifteen_odds)) %>%
  arrange(-top_pick_odds) %>%
  select(Team = team,
         `Top Pick Odds` = top_pick_odds,
         `2nd Pick Odds` = second_pick_odds,
         `3rd Pick Odds` = third_pick_odds,
         `4th Pick Odds` = fourth_pick_odds,
         `5th Pick Odds` = fifth_pick_odds,
         `Top 5 Picks Odds` = top_five_odds,
         `Top 10 Picks Odds` = top_ten_odds,
         `Top 15 Picks Odds` = top_fifteen_odds)
current_odds = read.csv('curr_odds.csv') %>%
  mutate(playoff_odds = percent(playoff_odds),
         division_odds = percent(division_odds),
         wcg_odds = percent(wcg_odds),
         wc_odds = percent(wc_odds),
         pennant_odds = percent(pennant_odds),
         ws_odds = percent(ws_odds),
         top_pick_odds = percent(top_pick_odds)) %>%
  select(c(Team = team,
           `Playoff Odds` = playoff_odds,
           `Division Odds` = division_odds,
           `WCG Odds` = wcg_odds,
           `WC Odds` = wc_odds,
           `DS Odds` = ds_odds,
           `Pennant Odds` = pennant_odds,
           `WS Odds` = ws_odds,
           `Top Pick Odds` = top_pick_odds))
standing_odds = read.csv('curr_odds.csv') %>%
  select(c(Team = team,
           `ROS Games` = RoSG,
           `Avg Wins` = AvgWins,
           `Median Wins` = MedianWins,
           `10th %ile Wins` = TenthPtileWins,
           `90th %ile Wins` = NinetiethPtileWins,
           `Minimum Wins` = MinWins,
           `Maximum Wins` = MaxWins))
elo_df = read.csv('elo_df.csv')
elo_df = elo_df %>%
  select(c(Team,
           League = League.....Lg,
           Record = Record.....Rec,
           `2031 Initial Elo` = X2031Start,
           `Season Elo Delta` = Seas....,
           `30 Day Elo` = X30.day,
           `30 Day Delta` = X30d...,
           `7 Day Elo` = X7.day,
           `7 Day Delta` = X7d...,
           `Current Elo` = Elo))

AL_East = c('BAL','NYY','TOR','TB','BOS')
AL_Central = c('CWS','DET','KC','MIN','CLE')
AL_West = c('HOU','SEA','TEX','LAA','OAK')
NL_East = c('MIA','NYM','ATL','WAS','PHI')
NL_Central = c('CHC','CIN','MIL','PIT','STL')
NL_West = c('ARI','COL','LAD','SD','SF')
NL = c(NL_East, NL_Central, NL_West)
AL = c(AL_East, AL_Central, AL_West)

team_colors = c("ARI" = "#A71930",
  "ATL" = "#CE1141",
  "BAL" = "#CE1141",
  "BOS" = "#BD3039",
  "CWS" = "#27251F",
  "CHC" = "#0E3386",
  "CIN" = "#C6011F",
  "CLE" = "#E31937",
  "COL" = "#33006F",
  "DET" = "#0C2340",
  "MIA" = "#00A3E0",
  "HOU" = "#EB6E1F",
  "KC" = "#004687",
  "LAA" = "#BA0021",
  "LAD" = "#005A9C",
  "MIL" = "#ffc52f",
  "MIN" = "#002B5C",
  "NYY" = "#003087",
  "NYM" = "#FF5910",
  "OAK" = "#003831",
  "PHI" = "#E81828",
  "PIT" = "#27251F",
  "SD" = "#2F241D",
  "SEA" = "#0C2C56",
  "SF" = "#FD5A1E",
  "STL" = "#C41E3A",
  "TB" = "#092C5C",
  "TEX" = "#003278",
  "TOR" = "#134A8E",
  "WAS" = "#AB0003")

# simmed_season_results = read_csv('all_sims.csv')
# records = read_csv('all_sim_records.csv')
  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Current Playoff Odds", tabName = "curr_odds", icon = icon("baseball-ball",lib="font-awesome")),
    menuItem("Current Draft Odds", tabName = "curr_draft_odds", icon = icon("calendar-alt",lib="font-awesome")),
    menuItem("Standings Projections", tabName = "standing_odds", icon = icon("table",lib="font-awesome")),
    menuItem("Elo Ratings", tabName = "curr_elo", icon = icon("chess-knight",lib="font-awesome")),
    menuItem("Playoff Odds Graph", tabName = "odds_over_time",icon = icon("chart-line",lib="font-awesome"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "curr_odds",
            h2("Current Playoff Odds"),
            DT::dataTableOutput("odds_dt")
    ),
    tabItem(tabName = "curr_draft_odds",
            h2("Current Draft Odds"),
            DT::dataTableOutput("draft_odds_dt")
    ),
    tabItem(tabName = "standing_odds",
            h2("Standings Projections"),
            dataTableOutput("wins_dt")
    ),
    tabItem(tabName = "curr_elo",
            h2("Current Elo Ratings"),
            dataTableOutput("elo_dt")
    ),
    tabItem(tabName = "odds_over_time",
            fluidRow(
              column(width = 4,
            h2("Playoff Odds Graph")),
            column(width = 4,
            pickerInput(
              inputId = "team_picker", 
              label = "Select team to view odds over time", 
              choices = elo_df$Team, 
              options = list(
                `actions-box` = TRUE, 
                size = 30,
                `selected-text-format` = "count > 3"
              ), 
              multiple = TRUE
            )),
            column(width = 4,
            pickerInput(
              inputId = "col_picker", 
              label = "Select odds  view over time", 
              choices =,c('Playoff Odds','Division Odds',
                          'WCG Odds','WC Odds','DS Odds','Pennant Odds','WS Odds','Top Pick Odds',
                          '2nd Pick Odds','3rd Pick Odds','4th Pick Odds','5th Pick Odds',
                          'Top 5 Picks Odds','Top 10 Picks Odds','Top 15 Picks Odds'), 
              options = list(
                `actions-box` = TRUE, 
                size = 30,
                `selected-text-format` = "count > 3"
              ), 
              multiple = F
            ))),
            fluidRow(
              column(width = 12,
            plotlyOutput('time_plot')
              )
            )
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "OBL Playoff Odds Tracker"),
    sidebar,
    body
  ),
  server = function(input, output, session) {
    output$elo_dt = renderDataTable(
      elo_df,
      rownames = F,
      options = list(pageLength = 30,
                     dom = 't')
    )
    
    output$odds_dt = DT::renderDataTable(
      datatable(current_odds,
                rownames = F,
                options = list(pageLength = 30,
                               dom = 't')) %>%
        formatPercentage(c('Playoff Odds','Division Odds',
        'WCG Odds','WC Odds','DS Odds','Pennant Odds','WS Odds','Top Pick Odds'),1)
    )
    
    output$draft_odds_dt = DT::renderDataTable(
      datatable(draft_odds,
                rownames = F,
                options = list(pageLength = 30,
                               dom = 't')) %>%
        formatPercentage(c('Top Pick Odds','2nd Pick Odds','3rd Pick Odds','4th Pick Odds','5th Pick Odds',
        'Top 5 Picks Odds','Top 10 Picks Odds','Top 15 Picks Odds'),1)
    )
    
    output$wins_dt = renderDataTable(
      standing_odds,
      rownames = F,
      options = list(pageLength = 30,
                     dom = 't')
    )
    
    plot_col = reactive({plyr::mapvalues(input$col_picker,c('Playoff Odds','Division Odds',
                                                  'WCG Odds','WC Odds','DS Odds','Pennant Odds','WS Odds','Top Pick Odds',
                                                  '2nd Pick Odds','3rd Pick Odds','4th Pick Odds','5th Pick Odds',
                                                  'Top 5 Picks Odds','Top 10 Picks Odds','Top 15 Picks Odds'),
                               c('playoff_odds','division_odds','wcg_odds','wc_odds','ds_odds','pennant_odds','ws_odds',
                                 'top_pick_odds','second_pick_odds','third_pick_odds','fourth_pick_odds','fifth_pick_odds',
                                 'top_five_odds','top_ten_odds','top_fifteen_odds'))})
    
    odds_graph = reactive(ggplot(data = odds_over_time %>%
                          filter(team %in% input$team_picker), aes_string(x = 'Week', y = plot_col(), group = 'team', color = 'team')) +
        geom_point() +
        geom_path() +
          xlim(1,31) +
          ylim(0,1) +
          labs(title = input$col_picker,
               xlab = 'Week',
               ylab = input$col_picker) +
          scale_color_manual(values = team_colors))
    
    output$time_plot = renderPlotly(ggplotly(odds_graph(),tooltip = c("team","Week",plot_col())))
  }
)
