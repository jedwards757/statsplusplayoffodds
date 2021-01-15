require(rvest)
require(dplyr)
require(foreach)
require(googlesheets4)
require(data.table)
require(elo)
require(lubridate)

elo_url = 'https://statsplus.net/oblootp/elo/current'

elo_df = read_html(elo_url) %>%
  html_node('table') %>%
  html_table() %>%
  mutate(Team = trimws(stringr::str_extract(Team, '[^   ]*$')))

team_names = read_html(elo_url) %>%
  html_node('table') %>%
  html_nodes('a') %>%
  html_text()

team_ids = read_html(elo_url) %>%
  html_node('table') %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  stringr::str_extract('\\d+')

id_map = data.frame(team_names,
                    team_ids) %>%
  slice(which(row_number() %% 2 == 0))

schedule_df = data.frame()

schedule_url = 'https://statsplus.net/oblootp/team/team_id?page=schedule'
for(id in id_map$team_ids){
  scrape_url = gsub('team_id',id, schedule_url)
  schedule_df = schedule_df %>%
    bind_rows(read_html(scrape_url) %>%
                html_node('table') %>%
                html_table() %>%
                slice(which(stringr::str_detect(Opp, '@',negate = T))) %>%
                mutate(HomeTeam = id_map$team_names[which(id_map$team_ids == id)]) %>%
                rename(AwayTeam = Opp))
}

schedule_df = schedule_df %>%
  left_join(elo_df %>%
              select(c(Team, Elo)),
            by=c('HomeTeam' = 'Team')) %>%
  left_join(elo_df %>%
              select(c(Team,Elo)),
            by=c('AwayTeam' = 'Team'),
            suffix = c('Home','Away')) %>%
  mutate(Date = stringr::str_replace(Date,'Aug.','August'),
         Date = stringr::str_replace(Date,'Sept.','September'),
         Date = stringr::str_replace(Date,'Oct.','October'),
         Date = lubridate::mdy(Date),
         Score = trimws(stringr::str_replace(Score,"\\*","")),
         HomeScore = as.numeric(stringr::str_extract(Score,'.+?(?= - )')),
         AwayScore = as.numeric(stringr::str_extract(Score, '[^ - ]*$')),
         HomeWpct = 1 / (1 + 10 ^ ((EloAway - (EloHome + 25)) / 400)),
         FinishedGameFlg = !is.na(HomeScore)) %>%
  arrange(Date)

compile_records = function(schedule_df, toDate = F){
  if(toDate){
    schedule_df = schedule_df %>%
      filter(FinishedGameFlg)
  }
  
  home_wins = schedule_df %>%
    group_by(HomeTeam) %>%
    summarize(HomeWins = sum(HomeScore > AwayScore), .groups = 'drop') %>%
    ungroup()
  
  away_wins = schedule_df %>%
    group_by(AwayTeam) %>%
    summarize(AwayWins = sum(AwayScore > HomeScore), .groups = 'drop') %>% 
    ungroup()
  
  home_losses = schedule_df %>%
    group_by(HomeTeam) %>%
    summarize(HomeLosses = sum(HomeScore < AwayScore), .groups = 'drop') %>%
    ungroup()
  
  away_losses = schedule_df %>%
    group_by(AwayTeam) %>%
    summarize(AwayLosses = sum(AwayScore < HomeScore), .groups = 'drop') %>% 
    ungroup()
  
  wins_df = home_wins %>%
    full_join(away_wins,by=c('HomeTeam'='AwayTeam')) %>%
    full_join(home_losses,by=c('HomeTeam'='HomeTeam')) %>%
    full_join(away_losses,by=c('HomeTeam'='AwayTeam'))
  
  wins_df[is.na(wins_df)] = 0
  
  wins_df %>%
    mutate(Wins = HomeWins + AwayWins,
           Losses = HomeLosses + AwayLosses,
           G = Wins + Losses,
           WPct=  Wins / G) %>%
    select(Team = HomeTeam,
           Wins, Losses, G, WPct) %>%
    arrange(-WPct)
           
}

sim_season = function(schedule_df, results = runif(2430), to_date = T, Overwrite = T){
  schedule_df$sim_prob = results
  if(to_date){
    schedule_df$HomeScoreSim = as.numeric(with(schedule_df, case_when(is.na(HomeScore)~HomeWpct > results,
                                                           T~HomeScore > AwayScore)))
  } else {
    schedule_df$HomeScoreSim = as.numeric(with(schedule_df, HomeWpct > results))
  }
  schedule_df$AwayScoreSim = 1 - schedule_df$HomeScoreSim
  schedule_df %>%
    mutate(HomeScore = HomeScoreSim,
           AwayScore = AwayScoreSim) %>%
    select(-c(HomeScoreSim, AwayScoreSim))
}

score_season = function(results_df){
  AL_East = c('BAL','NYY','TOR','TB','BOS')
  AL_Central = c('CWS','DET','KC','MIN','CLE')
  AL_West = c('HOU','SEA','TEX','LAA','OAK')
  NL_East = c('MIA','NYM','ATL','WAS','PHI')
  NL_Central = c('CHC','CIN','MIL','PIT','STL')
  NL_West = c('ARI','COL','LAD','SD','SF')
  NL = c(NL_East, NL_Central, NL_West)
  AL = c(AL_East, AL_Central, AL_West)
  
  AL_East_Winner = results_df %>%
    filter(Team %in% AL_East) %>%
    pull(Team) %>% 
    .[[1]]
  
  AL_East_Winner_Wins = results_df %>%
    filter(Team %in% AL_East) %>%
    pull(Wins) %>% 
    .[[1]]
  
  AL_Central_Winner = results_df %>%
    filter(Team %in% AL_Central) %>%
    pull(Team) %>% 
    .[[1]]
  
  AL_Central_Winner_Wins = results_df %>%
    filter(Team %in% AL_Central) %>%
    pull(Wins) %>% 
    .[[1]]
  
  AL_West_Winner = results_df %>%
    filter(Team %in% AL_West) %>%
    pull(Team) %>% 
    .[[1]]
  
  AL_West_Winner_Wins = results_df %>%
    filter(Team %in% AL_West) %>%
    pull(Wins) %>% 
    .[[1]]
  
  NL_East_Winner = results_df %>%
    filter(Team %in% NL_East) %>%
    pull(Team) %>% 
    .[[1]]
  
  NL_East_Winner_Wins = results_df %>%
    filter(Team %in% NL_East) %>%
    pull(Wins) %>% 
    .[[1]]
  
  NL_Central_Winner = results_df %>%
    filter(Team %in% NL_Central) %>%
    pull(Team) %>% 
    .[[1]]
  
  NL_Central_Winner_Wins = results_df %>%
    filter(Team %in% NL_Central) %>%
    pull(Wins) %>% 
    .[[1]]
  
  NL_West_Winner = results_df %>%
    filter(Team %in% NL_West) %>%
    pull(Team) %>% 
    .[[1]]
  
  NL_West_Winner_Wins = results_df %>%
    filter(Team %in% NL_West) %>%
    pull(Wins) %>% 
    .[[1]]
  
  NL_Div_Winners = c(NL_East_Winner, NL_Central_Winner, NL_West_Winner)
  AL_Div_Winners = c(AL_East_Winner, AL_Central_Winner, AL_West_Winner)
  
  NL_WC1_Winner = results_df %>%
    filter(!(Team %in% NL_Div_Winners) & Team %in% NL) %>%
    pull(Team) %>% 
    .[[1]]
  
  NL_WC1_Winner_Wins = results_df %>%
    filter(!(Team %in% NL_Div_Winners) & Team %in% NL) %>%
    pull(Wins) %>% 
    .[[1]]
  
  NL_WC2_Winner = results_df %>%
    filter(!(Team %in% NL_Div_Winners) & Team %in% NL) %>%
    pull(Team) %>% 
    .[[2]]
  
  NL_WC2_Winner_Wins = results_df %>%
    filter(!(Team %in% NL_Div_Winners) & Team %in% NL) %>%
    pull(Wins) %>% 
    .[[2]]
  
  AL_WC1_Winner = results_df %>%
    filter(!(Team %in% AL_Div_Winners) & Team %in% AL) %>%
    pull(Team) %>% 
    .[[1]]
  
  AL_WC1_Winner_Wins = results_df %>%
    filter(!(Team %in% AL_Div_Winners) & Team %in% AL) %>%
    pull(Wins) %>% 
    .[[1]]
  
  AL_WC2_Winner = results_df %>%
    filter(!(Team %in% AL_Div_Winners) & Team %in% AL) %>%
    pull(Team) %>% 
    .[[2]]
  
  AL_WC2_Winner_Wins = results_df %>%
    filter(!(Team %in% AL_Div_Winners) & Team %in% AL) %>%
    pull(Wins) %>% 
    .[[2]]
  
  Top_Pick = results_df %>%
    pull(Team) %>%
    .[[30]]
  
  Second_Pick = results_df %>%
    pull(Team) %>%
    .[[29]]
  
  Third_Pick = results_df %>%
    pull(Team) %>%
    .[[28]]
  
  Fourth_Pick = results_df %>%
    pull(Team) %>%
    .[[27]]
  
  Fifth_Pick = results_df %>%
    pull(Team) %>%
    .[[26]]
  
  Sixth_Pick = results_df %>%
    pull(Team) %>%
    .[[25]]
  
  Seventh_Pick = results_df %>%
    pull(Team) %>%
    .[[24]]
  
  Eighth_Pick = results_df %>%
    pull(Team) %>%
    .[[23]]
  
  Ninth_Pick = results_df %>%
    pull(Team) %>%
    .[[22]]
  
  Tenth_Pick = results_df %>%
    pull(Team) %>%
    .[[21]]
  
  Eleventh_Pick = results_df %>%
    pull(Team) %>%
    .[[20]]
  
  Twelfth_Pick = results_df %>%
    pull(Team) %>%
    .[[19]]
  
  Thirteenth_Pick = results_df %>%
    pull(Team) %>%
    .[[18]]
  
  Fourteenth_Pick = results_df %>%
    pull(Team) %>%
    .[[17]]
  
  Fifteenth_Pick = results_df %>%
    pull(Team) %>%
    .[[16]]
  
  data.frame(
    AL_East_Winner,
    AL_Central_Winner,
    AL_West_Winner,
    NL_East_Winner,
    NL_Central_Winner,
    NL_West_Winner,
    NL_WC1_Winner,
    NL_WC2_Winner,
    AL_WC1_Winner,
    AL_WC2_Winner,
    Top_Pick,
    AL_East_Winner_Wins,
    AL_Central_Winner_Wins,
    AL_West_Winner_Wins,
    NL_East_Winner_Wins,
    NL_Central_Winner_Wins,
    NL_West_Winner_Wins,
    NL_WC1_Winner_Wins,
    NL_WC2_Winner_Wins,
    AL_WC1_Winner_Wins,
    AL_WC2_Winner_Wins,
    Second_Pick,
    Third_Pick,
    Fourth_Pick,
    Fifth_Pick,
    Sixth_Pick,
    Seventh_Pick,
    Eighth_Pick,
    Ninth_Pick,
    Tenth_Pick,
    Eleventh_Pick,
    Twelfth_Pick,
    Thirteenth_Pick,
    Fourteenth_Pick,
    Fifteenth_Pick
  ) %>% as.list()
}

sim_playoffs = function(season_scores_list, elo_df, ASG_WIN = NA){
  # ALWC
  AL_WC_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_WC2_Winner)] - (elo_df$Elo[which(elo_df$Team == AL_WC1_Winner)] + 25)) / 400)),
                                                 AL_WC1_Winner,
                                                 AL_WC2_Winner))
  AL_WC_Winner_Wins = with(season_scores_list, ifelse(AL_WC_Winner == AL_WC1_Winner,AL_WC1_Winner_Wins,AL_WC2_Winner_Wins))
  
  # NLWC
  
  NL_WC_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_WC2_Winner)] - (elo_df$Elo[which(elo_df$Team == NL_WC1_Winner)] + 25)) / 400)),
                                                 NL_WC1_Winner,
                                                 NL_WC2_Winner))
  NL_WC_Winner_Wins = with(season_scores_list, ifelse(NL_WC_Winner == NL_WC1_Winner,NL_WC1_Winner_Wins,NL_WC2_Winner_Wins))
  
  # ALDS Seeding
  
  AL_Div_Wins = with(season_scores_list, c(AL_East_Winner_Wins, AL_Central_Winner_Wins, AL_West_Winner_Wins)) %>% sort()
  
  AL_Top_Seed = with(season_scores_list, case_when(AL_Div_Wins[3] == AL_East_Winner_Wins~AL_East_Winner,
                                                   AL_Div_Wins[3] == AL_Central_Winner_Wins~AL_Central_Winner,
                                                   AL_Div_Wins[3] == AL_West_Winner_Wins~AL_West_Winner,
                                                   T~NA_character_))
  
  AL_Second_Seed = with(season_scores_list, case_when(AL_Div_Wins[2] == AL_East_Winner_Wins & AL_East_Winner != AL_Top_Seed~AL_East_Winner,
                                                      AL_Div_Wins[2] == AL_Central_Winner_Wins & AL_Central_Winner != AL_Top_Seed~AL_Central_Winner,
                                                      AL_Div_Wins[2] == AL_West_Winner_Wins & AL_West_Winner != AL_Top_Seed~AL_West_Winner,
                                                      T~NA_character_))
  
  AL_Third_Seed = with(season_scores_list, case_when(AL_Div_Wins[1] == AL_East_Winner_Wins & AL_East_Winner != AL_Top_Seed & AL_East_Winner != AL_Second_Seed~AL_East_Winner,
                                                     AL_Div_Wins[1] == AL_Central_Winner_Wins & AL_Central_Winner != AL_Top_Seed & AL_Central_Winner != AL_Second_Seed~AL_Central_Winner,
                                                     AL_Div_Wins[1] == AL_West_Winner_Wins & AL_West_Winner != AL_Top_Seed & AL_West_Winner != AL_Second_Seed~AL_West_Winner,
                                                     T~NA_character_))
  
  # NLDS Seeding
  
  NL_Div_Wins = with(season_scores_list, c(NL_East_Winner_Wins, NL_Central_Winner_Wins, NL_West_Winner_Wins)) %>% sort()
  
  NL_Top_Seed = with(season_scores_list, case_when(NL_Div_Wins[3] == NL_East_Winner_Wins~NL_East_Winner,
                                                   NL_Div_Wins[3] == NL_Central_Winner_Wins~NL_Central_Winner,
                                                   NL_Div_Wins[3] == NL_West_Winner_Wins~NL_West_Winner,
                                                   T~NA_character_))
  
  NL_Second_Seed = with(season_scores_list, case_when(NL_Div_Wins[2] == NL_East_Winner_Wins & NL_East_Winner != NL_Top_Seed~NL_East_Winner,
                                                      NL_Div_Wins[2] == NL_Central_Winner_Wins & NL_Central_Winner != NL_Top_Seed~NL_Central_Winner,
                                                      NL_Div_Wins[2] == NL_West_Winner_Wins & NL_West_Winner != NL_Top_Seed~NL_West_Winner,
                                                      T~NA_character_))
  
  NL_Third_Seed = with(season_scores_list, case_when(NL_Div_Wins[1] == NL_East_Winner_Wins & NL_East_Winner != NL_Top_Seed & NL_East_Winner != NL_Second_Seed~NL_East_Winner,
                                                     NL_Div_Wins[1] == NL_Central_Winner_Wins & NL_Central_Winner != NL_Top_Seed & NL_Central_Winner != NL_Second_Seed~NL_Central_Winner,
                                                     NL_Div_Wins[1] == NL_West_Winner_Wins & NL_West_Winner != NL_Top_Seed & NL_West_Winner != NL_Second_Seed~NL_West_Winner,
                                                     T~NA_character_))
  
  # ALDS1
  AL_Top_Seed_Wins = 0
  AL_WC_Winner_Wins = 0
  ALDS1_Winner = ''
  while(TRUE){
    ALDS1_G1_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_WC_Winner)] - (elo_df$Elo[which(elo_df$Team == AL_Top_Seed)] + 25)) / 400)),
                                                      AL_Top_Seed,
                                                      AL_WC_Winner))
    AL_Top_Seed_Wins = AL_Top_Seed_Wins + as.numeric(AL_Top_Seed == ALDS1_G1_Winner)
    AL_WC_Winner_Wins = AL_WC_Winner_Wins + as.numeric(AL_WC_Winner == ALDS1_G1_Winner)
    
    ALDS1_G2_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_WC_Winner)] - (elo_df$Elo[which(elo_df$Team == AL_Top_Seed)] + 25)) / 400)),
                                                      AL_Top_Seed,
                                                      AL_WC_Winner))
    AL_Top_Seed_Wins = AL_Top_Seed_Wins + as.numeric(AL_Top_Seed == ALDS1_G2_Winner)
    AL_WC_Winner_Wins = AL_WC_Winner_Wins + as.numeric(AL_WC_Winner == ALDS1_G2_Winner)
    
    ALDS1_G3_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == AL_WC_Winner)] + 25)) / 400)),
                                                      AL_WC_Winner,
                                                      AL_Top_Seed))
    AL_Top_Seed_Wins = AL_Top_Seed_Wins + as.numeric(AL_Top_Seed == ALDS1_G3_Winner)
    AL_WC_Winner_Wins = AL_WC_Winner_Wins + as.numeric(AL_WC_Winner == ALDS1_G3_Winner)
    if(AL_Top_Seed_Wins == 3){
      ALDS1_Winner = AL_Top_Seed
      break
    } else if (AL_WC_Winner_Wins == 3){
      ALDS1_Winner = AL_WC_Winner
      break
    }
    
    ALDS1_G4_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == AL_WC_Winner)] + 25)) / 400)),
                                                      AL_WC_Winner,
                                                      AL_Top_Seed))
    AL_Top_Seed_Wins = AL_Top_Seed_Wins + as.numeric(AL_Top_Seed == ALDS1_G4_Winner)
    AL_WC_Winner_Wins = AL_WC_Winner_Wins + as.numeric(AL_WC_Winner == ALDS1_G4_Winner)
    if(AL_Top_Seed_Wins == 3){
      ALDS1_Winner = AL_Top_Seed
      break
    } else if (AL_WC_Winner_Wins == 3){
      ALDS1_Winner = AL_WC_Winner
      break
    }
    
    ALDS1_G5_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_WC_Winner)] - (elo_df$Elo[which(elo_df$Team == AL_Top_Seed)] + 25)) / 400)),
                                                      AL_Top_Seed,
                                                      AL_WC_Winner))
    AL_Top_Seed_Wins = AL_Top_Seed_Wins + as.numeric(AL_Top_Seed == ALDS1_G5_Winner)
    AL_WC_Winner_Wins = AL_WC_Winner_Wins + as.numeric(AL_WC_Winner == ALDS1_G5_Winner)
    if(AL_Top_Seed_Wins == 3){
      ALDS1_Winner = AL_Top_Seed
      break
    } else if (AL_WC_Winner_Wins == 3){
      ALDS1_Winner = AL_WC_Winner
      break
    } else {
      break
    }
  }
  
  # ALDS2
  AL_Second_Seed_Wins = 0
  AL_Third_Seed_Wins = 0
  ALDS2_Winner = ''
  while(TRUE){
    ALDS2_G1_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_Third_Seed)] - (elo_df$Elo[which(elo_df$Team == AL_Second_Seed)] + 25)) / 400)),
                                                      AL_Second_Seed,
                                                      AL_Third_Seed))
    AL_Second_Seed_Wins = AL_Second_Seed_Wins + as.numeric(AL_Second_Seed == ALDS2_G1_Winner)
    AL_Third_Seed_Wins = AL_Third_Seed_Wins + as.numeric(AL_Third_Seed == ALDS2_G1_Winner)
    
    ALDS2_G2_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_Third_Seed)] - (elo_df$Elo[which(elo_df$Team == AL_Second_Seed)] + 25)) / 400)),
                                                      AL_Second_Seed,
                                                      AL_Third_Seed))
    AL_Second_Seed_Wins = AL_Second_Seed_Wins + as.numeric(AL_Second_Seed == ALDS2_G2_Winner)
    AL_Third_Seed_Wins = AL_Third_Seed_Wins + as.numeric(AL_Third_Seed == ALDS2_G2_Winner)
    
    ALDS2_G3_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == AL_Third_Seed)] + 25)) / 400)),
                                                      AL_Third_Seed,
                                                      AL_Second_Seed))
    AL_Second_Seed_Wins = AL_Second_Seed_Wins + as.numeric(AL_Second_Seed == ALDS2_G3_Winner)
    AL_Third_Seed_Wins = AL_Third_Seed_Wins + as.numeric(AL_Third_Seed == ALDS2_G3_Winner)
    if(AL_Second_Seed_Wins == 3){
      ALDS2_Winner = AL_Second_Seed
      break
    } else if (AL_Third_Seed_Wins == 3){
      ALDS2_Winner = AL_Third_Seed
      break
    }
    
    ALDS2_G4_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == AL_Third_Seed)] + 25)) / 400)),
                                                      AL_Third_Seed,
                                                      AL_Second_Seed))
    AL_Second_Seed_Wins = AL_Second_Seed_Wins + as.numeric(AL_Second_Seed == ALDS2_G4_Winner)
    AL_Third_Seed_Wins = AL_Third_Seed_Wins + as.numeric(AL_Third_Seed == ALDS2_G4_Winner)
    if(AL_Second_Seed_Wins == 3){
      ALDS2_Winner = AL_Second_Seed
      break
    } else if (AL_Third_Seed_Wins == 3){
      ALDS2_Winner = AL_Third_Seed
      break
    }
    
    ALDS2_G5_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == AL_Third_Seed)] - (elo_df$Elo[which(elo_df$Team == AL_Second_Seed)] + 25)) / 400)),
                                                      AL_Second_Seed,
                                                      AL_Third_Seed))
    AL_Second_Seed_Wins = AL_Second_Seed_Wins + as.numeric(AL_Second_Seed == ALDS2_G5_Winner)
    AL_Third_Seed_Wins = AL_Third_Seed_Wins + as.numeric(AL_Third_Seed == ALDS2_G5_Winner)
    if(AL_Second_Seed_Wins == 3){
      ALDS2_Winner = AL_Second_Seed
      break
    } else if (AL_Third_Seed_Wins == 3){
      ALDS2_Winner = AL_Third_Seed
      break
    } else {
      break
    }
  }
  
  # NLDS1
  NL_Top_Seed_Wins = 0
  NL_WC_Winner_Wins = 0
  NLDS1_Winner = ''
  while(TRUE){
    NLDS1_G1_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_WC_Winner)] - (elo_df$Elo[which(elo_df$Team == NL_Top_Seed)] + 25)) / 400)),
                                                      NL_Top_Seed,
                                                      NL_WC_Winner))
    NL_Top_Seed_Wins = NL_Top_Seed_Wins + as.numeric(NL_Top_Seed == NLDS1_G1_Winner)
    NL_WC_Winner_Wins = NL_WC_Winner_Wins + as.numeric(NL_WC_Winner == NLDS1_G1_Winner)
    
    NLDS1_G2_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_WC_Winner)] - (elo_df$Elo[which(elo_df$Team == NL_Top_Seed)] + 25)) / 400)),
                                                      NL_Top_Seed,
                                                      NL_WC_Winner))
    NL_Top_Seed_Wins = NL_Top_Seed_Wins + as.numeric(NL_Top_Seed == NLDS1_G2_Winner)
    NL_WC_Winner_Wins = NL_WC_Winner_Wins + as.numeric(NL_WC_Winner == NLDS1_G2_Winner)
    
    NLDS1_G3_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == NL_WC_Winner)] + 25)) / 400)),
                                                      NL_WC_Winner,
                                                      NL_Top_Seed))
    NL_Top_Seed_Wins = NL_Top_Seed_Wins + as.numeric(NL_Top_Seed == NLDS1_G3_Winner)
    NL_WC_Winner_Wins = NL_WC_Winner_Wins + as.numeric(NL_WC_Winner == NLDS1_G3_Winner)
    if(NL_Top_Seed_Wins == 3){
      NLDS1_Winner = NL_Top_Seed
      break
    } else if (NL_WC_Winner_Wins == 3){
      NLDS1_Winner = NL_WC_Winner
      break
    }
    
    NLDS1_G4_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == NL_WC_Winner)] + 25)) / 400)),
                                                      NL_WC_Winner,
                                                      NL_Top_Seed))
    NL_Top_Seed_Wins = NL_Top_Seed_Wins + as.numeric(NL_Top_Seed == NLDS1_G4_Winner)
    NL_WC_Winner_Wins = NL_WC_Winner_Wins + as.numeric(NL_WC_Winner == NLDS1_G4_Winner)
    if(NL_Top_Seed_Wins == 3){
      NLDS1_Winner = NL_Top_Seed
      break
    } else if (NL_WC_Winner_Wins == 3){
      NLDS1_Winner = NL_WC_Winner
      break
    }
    
    NLDS1_G5_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_WC_Winner)] - (elo_df$Elo[which(elo_df$Team == NL_Top_Seed)] + 25)) / 400)),
                                                      NL_Top_Seed,
                                                      NL_WC_Winner))
    NL_Top_Seed_Wins = NL_Top_Seed_Wins + as.numeric(NL_Top_Seed == NLDS1_G5_Winner)
    NL_WC_Winner_Wins = NL_WC_Winner_Wins + as.numeric(NL_WC_Winner == NLDS1_G5_Winner)
    if(NL_Top_Seed_Wins == 3){
      NLDS1_Winner = NL_Top_Seed
      break
    } else if (NL_WC_Winner_Wins == 3){
      NLDS1_Winner = NL_WC_Winner
      break
    } else {
      break
    }
  }
  
  # NLDS2
  NL_Second_Seed_Wins = 0
  NL_Third_Seed_Wins = 0
  NLDS2_Winner = ''
  while(TRUE){
    NLDS2_G1_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_Third_Seed)] - (elo_df$Elo[which(elo_df$Team == NL_Second_Seed)] + 25)) / 400)),
                                                      NL_Second_Seed,
                                                      NL_Third_Seed))
    NL_Second_Seed_Wins = NL_Second_Seed_Wins + as.numeric(NL_Second_Seed == NLDS2_G1_Winner)
    NL_Third_Seed_Wins = NL_Third_Seed_Wins + as.numeric(NL_Third_Seed == NLDS2_G1_Winner)
    
    NLDS2_G2_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_Third_Seed)] - (elo_df$Elo[which(elo_df$Team == NL_Second_Seed)] + 25)) / 400)),
                                                      NL_Second_Seed,
                                                      NL_Third_Seed))
    NL_Second_Seed_Wins = NL_Second_Seed_Wins + as.numeric(NL_Second_Seed == NLDS2_G2_Winner)
    NL_Third_Seed_Wins = NL_Third_Seed_Wins + as.numeric(NL_Third_Seed == NLDS2_G2_Winner)
    
    NLDS2_G3_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == NL_Third_Seed)] + 25)) / 400)),
                                                      NL_Third_Seed,
                                                      NL_Second_Seed))
    NL_Second_Seed_Wins = NL_Second_Seed_Wins + as.numeric(NL_Second_Seed == NLDS2_G3_Winner)
    NL_Third_Seed_Wins = NL_Third_Seed_Wins + as.numeric(NL_Third_Seed == NLDS2_G3_Winner)
    if(NL_Second_Seed_Wins == 3){
      NLDS2_Winner = NL_Second_Seed
      break
    } else if (NL_Third_Seed_Wins == 3){
      NLDS2_Winner = NL_Third_Seed
      break
    }
    
    NLDS2_G4_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == NL_Third_Seed)] + 25)) / 400)),
                                                      NL_Third_Seed,
                                                      NL_Second_Seed))
    NL_Second_Seed_Wins = NL_Second_Seed_Wins + as.numeric(NL_Second_Seed == NLDS2_G4_Winner)
    NL_Third_Seed_Wins = NL_Third_Seed_Wins + as.numeric(NL_Third_Seed == NLDS2_G4_Winner)
    if(NL_Second_Seed_Wins == 3){
      NLDS2_Winner = NL_Second_Seed
      break
    } else if (NL_Third_Seed_Wins == 3){
      NLDS2_Winner = NL_Third_Seed
      break
    }
    
    NLDS2_G5_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NL_Third_Seed)] - (elo_df$Elo[which(elo_df$Team == NL_Second_Seed)] + 25)) / 400)),
                                                      NL_Second_Seed,
                                                      NL_Third_Seed))
    NL_Second_Seed_Wins = NL_Second_Seed_Wins + as.numeric(NL_Second_Seed == NLDS2_G5_Winner)
    NL_Third_Seed_Wins = NL_Third_Seed_Wins + as.numeric(NL_Third_Seed == NLDS2_G5_Winner)
    if(NL_Second_Seed_Wins == 3){
      NLDS2_Winner = NL_Second_Seed
      break
    } else if (NL_Third_Seed_Wins == 3){
      NLDS2_Winner = NL_Third_Seed
      break
    } else {
      break
    }
  }
  
  # ALCS
  ALCS_Top_Seed = case_when(ALDS1_Winner == AL_Top_Seed~ALDS1_Winner,
                            ALDS2_Winner == AL_Top_Seed~ALDS2_Winner,
                            ALDS1_Winner == AL_Second_Seed~ALDS1_Winner,
                            ALDS2_Winner == AL_Second_Seed~ALDS2_Winner,
                            ALDS1_Winner == AL_Third_Seed~ALDS1_Winner,
                            ALDS2_Winner == AL_Third_Seed~ALDS2_Winner,
                            ALDS1_Winner == AL_WC_Winner~AL_WC_Winner,
                            ALDS2_Winner == AL_WC_Winner~AL_WC_Winner,
                            T~NA_character_)
  ALCS_Second_Seed = case_when(ALCS_Top_Seed == ALDS1_Winner ~ ALDS2_Winner,
                               T~ALDS1_Winner)
  
  ALCS_Top_Seed_Wins = 0
  ALCS_Second_Seed_Wins = 0
  ALCS_Winner = ''
  while(TRUE){
    ALCS_G1_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == ALCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == ALCS_Top_Seed)] + 25)) / 400)),
                                                     ALCS_Top_Seed,
                                                     ALCS_Second_Seed))
    ALCS_Top_Seed_Wins = ALCS_Top_Seed_Wins + as.numeric(ALCS_Top_Seed== ALCS_G1_Winner)
    ALCS_Second_Seed_Wins = ALCS_Second_Seed_Wins + as.numeric(ALCS_Second_Seed == ALCS_G1_Winner)
    
    ALCS_G2_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == ALCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == ALCS_Top_Seed)] + 25)) / 400)),
                                                     ALCS_Top_Seed,
                                                     ALCS_Second_Seed))
    ALCS_Top_Seed_Wins = ALCS_Top_Seed_Wins + as.numeric(ALCS_Top_Seed== ALCS_G2_Winner)
    ALCS_Second_Seed_Wins = ALCS_Second_Seed_Wins + as.numeric(ALCS_Second_Seed == ALCS_G2_Winner)
    
    ALCS_G3_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == ALCS_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == ALCS_Second_Seed)] + 25)) / 400)),
                                                     ALCS_Second_Seed,
                                                     ALCS_Top_Seed))
    ALCS_Top_Seed_Wins = ALCS_Top_Seed_Wins + as.numeric(ALCS_Top_Seed== ALCS_G3_Winner)
    ALCS_Second_Seed_Wins = ALCS_Second_Seed_Wins + as.numeric(ALCS_Second_Seed == ALCS_G3_Winner)
    
    ALCS_G4_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == ALCS_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == ALCS_Second_Seed)] + 25)) / 400)),
                                                     ALCS_Second_Seed,
                                                     ALCS_Top_Seed))
    ALCS_Top_Seed_Wins = ALCS_Top_Seed_Wins + as.numeric(ALCS_Top_Seed== ALCS_G4_Winner)
    ALCS_Second_Seed_Wins = ALCS_Second_Seed_Wins + as.numeric(ALCS_Second_Seed == ALCS_G4_Winner)
    if(ALCS_Top_Seed_Wins == 4){
      ALCS_Winner = ALCS_Top_Seed
      break
    } else if (ALCS_Second_Seed_Wins == 4){
      ALCS_Winner = ALCS_Second_Seed
      break
    }
    
    ALCS_G5_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == ALCS_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == ALCS_Second_Seed)] + 25)) / 400)),
                                                     ALCS_Second_Seed,
                                                     ALCS_Top_Seed))
    ALCS_Top_Seed_Wins = ALCS_Top_Seed_Wins + as.numeric(ALCS_Top_Seed== ALCS_G5_Winner)
    ALCS_Second_Seed_Wins = ALCS_Second_Seed_Wins + as.numeric(ALCS_Second_Seed == ALCS_G5_Winner)
    if(ALCS_Top_Seed_Wins == 4){
      ALCS_Winner = ALCS_Top_Seed
      break
    } else if (ALCS_Second_Seed_Wins == 4){
      ALCS_Winner = ALCS_Second_Seed
      break
    }
    
    ALCS_G6_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == ALCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == ALCS_Top_Seed)] + 25)) / 400)),
                                                     ALCS_Top_Seed,
                                                     ALCS_Second_Seed))
    ALCS_Top_Seed_Wins = ALCS_Top_Seed_Wins + as.numeric(ALCS_Top_Seed== ALCS_G6_Winner)
    ALCS_Second_Seed_Wins = ALCS_Second_Seed_Wins + as.numeric(ALCS_Second_Seed == ALCS_G6_Winner)
    if(ALCS_Top_Seed_Wins == 4){
      ALCS_Winner = ALCS_Top_Seed
      break
    } else if (ALCS_Second_Seed_Wins == 4){
      ALCS_Winner = ALCS_Second_Seed
      break
    }
    
    ALCS_G7_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == ALCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == ALCS_Top_Seed)] + 25)) / 400)),
                                                     ALCS_Top_Seed,
                                                     ALCS_Second_Seed))
    ALCS_Top_Seed_Wins = ALCS_Top_Seed_Wins + as.numeric(ALCS_Top_Seed== ALCS_G7_Winner)
    ALCS_Second_Seed_Wins = ALCS_Second_Seed_Wins + as.numeric(ALCS_Second_Seed == ALCS_G7_Winner)
    if(ALCS_Top_Seed_Wins == 4){
      ALCS_Winner = ALCS_Top_Seed
      break
    } else if (ALCS_Second_Seed_Wins == 4){
      ALCS_Winner = ALCS_Second_Seed
      break
    } else {
      break
    }
  }
  
  # NLCS
  NLCS_Top_Seed = case_when(NLDS1_Winner == NL_Top_Seed~NLDS1_Winner,
                            NLDS2_Winner == NL_Top_Seed~NLDS2_Winner,
                            NLDS1_Winner == NL_Second_Seed~NLDS1_Winner,
                            NLDS2_Winner == NL_Second_Seed~NLDS2_Winner,
                            NLDS1_Winner == NL_Third_Seed~NLDS1_Winner,
                            NLDS2_Winner == NL_Third_Seed~NLDS2_Winner,
                            NLDS1_Winner == NL_WC_Winner~NL_WC_Winner,
                            NLDS2_Winner == NL_WC_Winner~NL_WC_Winner,
                            T~NA_character_)
  NLCS_Second_Seed = case_when(NLCS_Top_Seed == NLDS1_Winner ~ NLDS2_Winner,
                               T~NLDS1_Winner)
  
  NLCS_Top_Seed_Wins = 0
  NLCS_Second_Seed_Wins = 0
  NLCS_Winner = ''
  while(TRUE){
    NLCS_G1_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NLCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == NLCS_Top_Seed)] + 25)) / 400)),
                                                     NLCS_Top_Seed,
                                                     NLCS_Second_Seed))
    NLCS_Top_Seed_Wins = NLCS_Top_Seed_Wins + as.numeric(NLCS_Top_Seed== NLCS_G1_Winner)
    NLCS_Second_Seed_Wins = NLCS_Second_Seed_Wins + as.numeric(NLCS_Second_Seed == NLCS_G1_Winner)
    
    NLCS_G2_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NLCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == NLCS_Top_Seed)] + 25)) / 400)),
                                                     NLCS_Top_Seed,
                                                     NLCS_Second_Seed))
    NLCS_Top_Seed_Wins = NLCS_Top_Seed_Wins + as.numeric(NLCS_Top_Seed== NLCS_G2_Winner)
    NLCS_Second_Seed_Wins = NLCS_Second_Seed_Wins + as.numeric(NLCS_Second_Seed == NLCS_G2_Winner)
    
    NLCS_G3_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NLCS_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == NLCS_Second_Seed)] + 25)) / 400)),
                                                     NLCS_Second_Seed,
                                                     NLCS_Top_Seed))
    NLCS_Top_Seed_Wins = NLCS_Top_Seed_Wins + as.numeric(NLCS_Top_Seed== NLCS_G3_Winner)
    NLCS_Second_Seed_Wins = NLCS_Second_Seed_Wins + as.numeric(NLCS_Second_Seed == NLCS_G3_Winner)
    
    NLCS_G4_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NLCS_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == NLCS_Second_Seed)] + 25)) / 400)),
                                                     NLCS_Second_Seed,
                                                     NLCS_Top_Seed))
    NLCS_Top_Seed_Wins = NLCS_Top_Seed_Wins + as.numeric(NLCS_Top_Seed== NLCS_G4_Winner)
    NLCS_Second_Seed_Wins = NLCS_Second_Seed_Wins + as.numeric(NLCS_Second_Seed == NLCS_G4_Winner)
    if(NLCS_Top_Seed_Wins == 4){
      NLCS_Winner = NLCS_Top_Seed
      break
    } else if (NLCS_Second_Seed_Wins == 4){
      NLCS_Winner = NLCS_Second_Seed
      break
    }
    
    NLCS_G5_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NLCS_Top_Seed)] - (elo_df$Elo[which(elo_df$Team == NLCS_Second_Seed)] + 25)) / 400)),
                                                     NLCS_Second_Seed,
                                                     NLCS_Top_Seed))
    NLCS_Top_Seed_Wins = NLCS_Top_Seed_Wins + as.numeric(NLCS_Top_Seed== NLCS_G5_Winner)
    NLCS_Second_Seed_Wins = NLCS_Second_Seed_Wins + as.numeric(NLCS_Second_Seed == NLCS_G5_Winner)
    if(NLCS_Top_Seed_Wins == 4){
      NLCS_Winner = NLCS_Top_Seed
      break
    } else if (NLCS_Second_Seed_Wins == 4){
      NLCS_Winner = NLCS_Second_Seed
      break
    }
    
    NLCS_G6_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NLCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == NLCS_Top_Seed)] + 25)) / 400)),
                                                     NLCS_Top_Seed,
                                                     NLCS_Second_Seed))
    NLCS_Top_Seed_Wins = NLCS_Top_Seed_Wins + as.numeric(NLCS_Top_Seed== NLCS_G6_Winner)
    NLCS_Second_Seed_Wins = NLCS_Second_Seed_Wins + as.numeric(NLCS_Second_Seed == NLCS_G6_Winner)
    if(NLCS_Top_Seed_Wins == 4){
      NLCS_Winner = NLCS_Top_Seed
      break
    } else if (NLCS_Second_Seed_Wins == 4){
      NLCS_Winner = NLCS_Second_Seed
      break
    }
    
    NLCS_G7_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == NLCS_Second_Seed)] - (elo_df$Elo[which(elo_df$Team == NLCS_Top_Seed)] + 25)) / 400)),
                                                     NLCS_Top_Seed,
                                                     NLCS_Second_Seed))
    NLCS_Top_Seed_Wins = NLCS_Top_Seed_Wins + as.numeric(NLCS_Top_Seed== NLCS_G7_Winner)
    NLCS_Second_Seed_Wins = NLCS_Second_Seed_Wins + as.numeric(NLCS_Second_Seed == NLCS_G7_Winner)
    if(NLCS_Top_Seed_Wins == 4){
      NLCS_Winner = NLCS_Top_Seed
      break
    } else if (NLCS_Second_Seed_Wins == 4){
      NLCS_Winner = NLCS_Second_Seed
      break
    } else {
      break
    }
  }
  
  if(is.na(ASG_WIN)){
    if(runif(1) > 0.5){
      Top_Seed = NLCS_Winner
      Second_Seed = ALCS_Winner
    } else {
      Top_Seed = ALCS_Winner
      Second_Seed = NLCS_Winner
    }
  } else if(ASG_WIN == 'NL'){
    Top_Seed = NLCS_Winner
    Second_Seed = ALCS_Winner
  } else {
    Top_Seed = ALCS_Winner
    Second_Seed = NLCS_Winner
  }
  
  Top_Seed_Wins = 0
  Second_Seed_Wins = 0
  Winner = ''
  while(TRUE){
    WS_G1_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == Second_Seed)] - (elo_df$Elo[which(elo_df$Team == Top_Seed)] + 25)) / 400)),
                                                   Top_Seed,
                                                   Second_Seed))
    Top_Seed_Wins = Top_Seed_Wins + as.numeric(Top_Seed== WS_G1_Winner)
    Second_Seed_Wins = Second_Seed_Wins + as.numeric(Second_Seed == WS_G1_Winner)
    
    WS_G2_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == Second_Seed)] - (elo_df$Elo[which(elo_df$Team == Top_Seed)] + 25)) / 400)),
                                                   Top_Seed,
                                                   Second_Seed))
    Top_Seed_Wins = Top_Seed_Wins + as.numeric(Top_Seed== WS_G2_Winner)
    Second_Seed_Wins = Second_Seed_Wins + as.numeric(Second_Seed == WS_G2_Winner)
    
    WS_G3_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == Top_Seed)] - (elo_df$Elo[which(elo_df$Team == Second_Seed)] + 25)) / 400)),
                                                   Second_Seed,
                                                   Top_Seed))
    Top_Seed_Wins = Top_Seed_Wins + as.numeric(Top_Seed== WS_G3_Winner)
    Second_Seed_Wins = Second_Seed_Wins + as.numeric(Second_Seed == WS_G3_Winner)
    
    WS_G4_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == Top_Seed)] - (elo_df$Elo[which(elo_df$Team == Second_Seed)] + 25)) / 400)),
                                                   Second_Seed,
                                                   Top_Seed))
    Top_Seed_Wins = Top_Seed_Wins + as.numeric(Top_Seed== WS_G4_Winner)
    Second_Seed_Wins = Second_Seed_Wins + as.numeric(Second_Seed == WS_G4_Winner)
    if(Top_Seed_Wins == 4){
      Winner = Top_Seed
      break
    } else if (Second_Seed_Wins == 4){
      Winner = Second_Seed
      break
    }
    
    WS_G5_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == Top_Seed)] - (elo_df$Elo[which(elo_df$Team == Second_Seed)] + 25)) / 400)),
                                                   Second_Seed,
                                                   Top_Seed))
    Top_Seed_Wins = Top_Seed_Wins + as.numeric(Top_Seed== WS_G5_Winner)
    Second_Seed_Wins = Second_Seed_Wins + as.numeric(Second_Seed == WS_G5_Winner)
    if(Top_Seed_Wins == 4){
      Winner = Top_Seed
      break
    } else if (Second_Seed_Wins == 4){
      Winner = Second_Seed
      break
    }
    
    WS_G6_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == Second_Seed)] - (elo_df$Elo[which(elo_df$Team == Top_Seed)] + 25)) / 400)),
                                                   Top_Seed,
                                                   Second_Seed))
    Top_Seed_Wins = Top_Seed_Wins + as.numeric(Top_Seed== WS_G6_Winner)
    Second_Seed_Wins = Second_Seed_Wins + as.numeric(Second_Seed == WS_G6_Winner)
    if(Top_Seed_Wins == 4){
      Winner = Top_Seed
      break
    } else if (Second_Seed_Wins == 4){
      Winner = Second_Seed
      break
    }
    
    WS_G7_Winner = with(season_scores_list, ifelse(runif(1) < 1 / (1 + 10 ^ ((elo_df$Elo[which(elo_df$Team == Second_Seed)] - (elo_df$Elo[which(elo_df$Team == Top_Seed)] + 25)) / 400)),
                                                   Top_Seed,
                                                   Second_Seed))
    Top_Seed_Wins = Top_Seed_Wins + as.numeric(Top_Seed== WS_G7_Winner)
    Second_Seed_Wins = Second_Seed_Wins + as.numeric(Second_Seed == WS_G7_Winner)
    if(Top_Seed_Wins == 4){
      Winner = Top_Seed
      break
    } else if (Second_Seed_Wins == 4){
      Winner = Second_Seed
      break
    } else {
      break
    }
  }
  
  data.frame(
    AL_WC1_Winner = season_scores_list$AL_WC1_Winner,
    AL_WC2_Winner = season_scores_list$AL_WC2_Winner,
    NL_WC1_Winner = season_scores_list$NL_WC1_Winner,
    NL_WC2_Winner = season_scores_list$NL_WC2_Winner,
    AL_East_Winner = season_scores_list$AL_East_Winner,
    AL_Central_Winner = season_scores_list$AL_Central_Winner,
    AL_West_Winner = season_scores_list$AL_West_Winner,
    NL_East_Winner = season_scores_list$NL_East_Winner,
    NL_Central_Winner = season_scores_list$NL_Central_Winner,
    NL_West_Winner = season_scores_list$NL_West_Winner,
    Top_Pick = season_scores_list$Top_Pick,
    Second_Pick =  season_scores_list$Second_Pick,
    Third_Pick =  season_scores_list$Third_Pick,
    Fourth_Pick =  season_scores_list$Fourth_Pick,
    Fifth_Pick =  season_scores_list$Fifth_Pick,
    Sixth_Pick =  season_scores_list$Sixth_Pick,
    Seventh_Pick =  season_scores_list$Seventh_Pick,
    Eighth_Pick =  season_scores_list$Eighth_Pick,
    Ninth_Pick =  season_scores_list$Ninth_Pick,
    Tenth_Pick = season_scores_list$Tenth_Pick,
    Eleventh_Pick =  season_scores_list$Eleventh_Pick,
    Twelfth_Pick =  season_scores_list$Twelfth_Pick,
    Thirteenth_Pick = season_scores_list$Thirteenth_Pick,
    Fourteenth_Pick =  season_scores_list$Fourteenth_Pick,
    Fifteenth_Pick =  season_scores_list$Fifteenth_Pick,
    AL_WC_Winner,
    NL_WC_Winner,
    ALDS1_Winner,
    ALDS2_Winner,
    NLDS1_Winner,
    NLDS2_Winner,
    ALCS_Winner,
    NLCS_Winner,
    Winner
  )
}

sims = 10000

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

simmed_season_result_list = foreach(i = c(1:sims),
                                    .combine='comb', 
                                    .multicombine=TRUE,
                                    .init=list(list(), list()),
                                    .packages = c('dplyr')) %do% {
  season = sim_season(schedule_df) %>% compile_records() %>% mutate(sim = i)
  playoff_results = season %>% score_season() %>% sim_playoffs(elo_df) %>% mutate(sim = i)
  
  list(season,playoff_results)
                                    }

records = rbindlist(simmed_season_result_list[[1]])
simmed_season_results = rbindlist(simmed_season_result_list[[2]])


odds_df = data.frame()

AL_East = c('BAL','NYY','TOR','TB','BOS')
AL_Central = c('CWS','DET','KC','MIN','CLE')
AL_West = c('HOU','SEA','TEX','LAA','OAK')
NL_East = c('MIA','NYM','ATL','WAS','PHI')
NL_Central = c('CHC','CIN','MIL','PIT','STL')
NL_West = c('ARI','COL','LAD','SD','SF')
NL = c(NL_East, NL_Central, NL_West)
AL = c(AL_East, AL_Central, AL_West)

for(team in id_map$team_names){
  if(team %in% NL){
    team_records = records %>%
      filter(Team == team) %>%
      summarize(AvgWins = mean(Wins),
                MedianWins = median(Wins),
                TenthPtileWins = round(quantile(Wins, 0.1)),
                NinetiethPtileWins = round(quantile(Wins,0.9)),
                MinWins = min(Wins),
                MaxWins = max(Wins))
    
    playoff_odds = with(simmed_season_results, sum(team == NL_WC1_Winner) +
                          sum(team == NL_WC2_Winner) +
                          sum(team == NL_East_Winner) +
                          sum(team == NL_Central_Winner) +
                          sum(team == NL_West_Winner)) / sims
    division_odds = with(simmed_season_results, case_when(team %in% NL_East ~ sum(team == NL_East_Winner),
                                                          team %in% NL_Central ~ sum(team == NL_Central_Winner),
                                                          team %in% NL_West ~ sum(team == NL_West_Winner),
                                                          T~NA_integer_)) / sims
    wcg_odds = with(simmed_season_results, sum(team == NL_WC1_Winner) +
                     sum(team == NL_WC2_Winner)) / sims
    wc_odds = with(simmed_season_results, sum(team == NL_WC_Winner)) / sims
    ds_odds = with(simmed_season_results, sum(team == NLDS1_Winner) + sum(team == NLDS2_Winner)) / sims
    pennant_odds = with(simmed_season_results, sum(team == NLCS_Winner)) / sims
    ws_odds = with(simmed_season_results, sum(team == Winner)) / sims
    top_pick_odds = with(simmed_season_results, sum(team == Top_Pick)) / sims
    second_pick_odds = with(simmed_season_results, sum(team == Second_Pick)) / sims
    third_pick_odds = with(simmed_season_results, sum(team == Third_Pick)) / sims
    fourth_pick_odds = with(simmed_season_results, sum(team == Fourth_Pick)) / sims
    fifth_pick_odds = with(simmed_season_results, sum(team == Fifth_Pick)) / sims
    top_five_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick)) / sims
    top_ten_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                     team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick)) / sims
    top_fifteen_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                     team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick | 
                                                       team == Eleventh_Pick | team == Twelfth_Pick | team == Thirteenth_Pick | team == Fourteenth_Pick | team == Fifteenth_Pick)) / sims
    
    
    # RoS WPCT
    
    RoS = schedule_df %>%
      filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
      summarize(RoSG = n(),
                RoSxW = sum(case_when(team == HomeTeam~HomeWpct,
                                   T~1-HomeWpct))) %>%
      mutate(RoSxWPct = round(RoSxW / RoSG,3))
    
    # SoS
    SoS = schedule_df %>%
      filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
      mutate(EloHome = case_when(HomeTeam == team~1500,
                                 T~EloHome),
             EloAway = case_when(AwayTeam == team~1500,
                                 T~EloAway),
             HomeWpct = 1 / (1 + 10 ^ ((EloAway - (EloHome + 25)) / 400))) %>%
      summarize(RoSG = n(),
                NeutxW = sum(case_when(team == HomeTeam~HomeWpct,
                                      T~1-HomeWpct))) %>%
      mutate(SoS = round(NeutxW / RoSG,3))
    
    odds_df = odds_df %>%
      bind_rows(
        data.frame(
          team,
          playoff_odds,
          division_odds,
          wcg_odds,
          wc_odds,
          ds_odds,
          pennant_odds,
          ws_odds,
          top_pick_odds,
          second_pick_odds,
          third_pick_odds,
          fourth_pick_odds,
          fifth_pick_odds,
          top_five_odds,
          top_ten_odds,
          top_fifteen_odds,
          RoSG = RoS$RoSG,
          RoSxWPct = RoS$RoSxWPct,
          SoS = SoS$SoS
      ) %>% cbind(team_records)
    )
    
  } else {
    team_records = records %>%
      filter(Team == team) %>%
      summarize(AvgWins = mean(Wins),
                MedianWins = median(Wins),
                TenthPtileWins = round(quantile(Wins, 0.1)),
                NinetiethPtileWins = round(quantile(Wins,0.9)),
                MinWins = min(Wins),
                MaxWins = max(Wins))
    
    playoff_odds = with(simmed_season_results, sum(team == AL_WC1_Winner) +
                          sum(team == AL_WC2_Winner) +
                          sum(team == AL_East_Winner) +
                          sum(team == AL_Central_Winner) +
                          sum(team == AL_West_Winner)) / sims
    division_odds = with(simmed_season_results, case_when(team %in% AL_East ~ sum(team == AL_East_Winner),
                                                          team %in% AL_Central ~ sum(team == AL_Central_Winner),
                                                          team %in% AL_West ~ sum(team == AL_West_Winner),
                                                          T~NA_integer_)) / sims
    wcg_odds = with(simmed_season_results, sum(team == AL_WC1_Winner) +
                      sum(team == AL_WC2_Winner)) / sims
    wc_odds = with(simmed_season_results, sum(team == AL_WC_Winner)) / sims
    ds_odds = with(simmed_season_results, sum(team == ALDS1_Winner) + sum(team == ALDS2_Winner)) / sims
    pennant_odds = with(simmed_season_results, sum(team == ALCS_Winner)) / sims
    ws_odds = with(simmed_season_results, sum(team == Winner)) / sims
    top_pick_odds = with(simmed_season_results, sum(team == Top_Pick)) / sims
    second_pick_odds = with(simmed_season_results, sum(team == Second_Pick)) / sims
    third_pick_odds = with(simmed_season_results, sum(team == Third_Pick)) / sims
    fourth_pick_odds = with(simmed_season_results, sum(team == Fourth_Pick)) / sims
    fifth_pick_odds = with(simmed_season_results, sum(team == Fifth_Pick)) / sims
    top_five_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick)) / sims
    top_ten_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                     team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick)) / sims
    top_fifteen_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                         team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick | 
                                                         team == Eleventh_Pick | team == Twelfth_Pick | team == Thirteenth_Pick | team == Fourteenth_Pick | team == Fifteenth_Pick)) / sims
    
    # RoS WPCT
    
    RoS = schedule_df %>%
      filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
      summarize(RoSG = n(),
                RoSxW = sum(case_when(team == HomeTeam~HomeWpct,
                                      T~1-HomeWpct))) %>%
      mutate(RoSxWPct = round(RoSxW / RoSG,3))
    
    # SoS
    SoS = schedule_df %>%
      filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
      mutate(EloHome = case_when(HomeTeam == team~1500,
                                 T~EloHome),
             EloAway = case_when(AwayTeam == team~1500,
                                 T~EloAway),
             HomeWpct = 1 / (1 + 10 ^ ((EloAway - (EloHome + 25)) / 400))) %>%
      summarize(RoSG = n(),
                NeutxW = sum(case_when(team == HomeTeam~HomeWpct,
                                       T~1-HomeWpct))) %>%
      mutate(SoS = round(NeutxW / RoSG,3))
    
    odds_df = odds_df %>%
      bind_rows(
        data.frame(
          team,
          playoff_odds,
          division_odds,
          wcg_odds,
          wc_odds,
          ds_odds,
          pennant_odds,
          ws_odds,
          top_pick_odds,
          second_pick_odds,
          third_pick_odds,
          fourth_pick_odds,
          fifth_pick_odds,
          top_five_odds,
          top_ten_odds,
          top_fifteen_odds,
          RoSG = RoS$RoSG,
          RoSxWPct = RoS$RoSxWPct,
          SoS = SoS$SoS
        ) %>% cbind(team_records)
      )
  }
}

# generate playoff odds over time

initial.elos = elo_df[,5]
names(initial.elos) = elo_df$Team

elo_df_temp = elo_df
odds_over_time = data.frame()

weeks = unique(week(schedule_df$Date[which(schedule_df$FinishedGameFlg)]))

# first date: '2031-04-04'
# for(date in unique(as.character(schedule_df$Date[which(schedule_df$FinishedGameFlg)]))){
for(week in weeks){
  print(week)
  finished_games = schedule_df %>%
    filter(week(Date) <= week & FinishedGameFlg)
  incomplete_games = schedule_df %>%
    filter((week(Date) == week & !FinishedGameFlg) | week(Date) > week) %>%
    mutate(HomeScore = NA_real_)
  elo.run = elo.run(score(HomeScore,AwayScore) ~ adjust(HomeTeam,25) + AwayTeam + k(5*log(abs(HomeScore - AwayScore) + 1)), data =finished_games, initial.elos = initial.elos)
  final_elos = final.elos(elo.run)
  incomplete_games$HomeWpct = predict(elo.run, incomplete_games)
  temp_schedule = bind_rows(finished_games, incomplete_games)
  
  elo_df_temp$Elo = plyr::mapvalues(elo_df_temp$Team, names(final_elos), final_elos) %>% as.numeric()
  
  sims = 1000
  simmed_season_result_list = foreach(i = c(1:sims),
                                      .combine='comb', 
                                      .multicombine=TRUE,
                                      .init=list(list(), list()),
                                      .packages = c('dplyr')) %do% {
                                        season = sim_season(temp_schedule ) %>% compile_records()
                                        playoff_results = season %>% score_season() %>% sim_playoffs(elo_df_temp)
                                        
                                        list(season,playoff_results)
                                      }
  
  records = rbindlist(simmed_season_result_list[[1]])
  simmed_season_results = rbindlist(simmed_season_result_list[[2]])
  
  
  odds_df = data.frame()
  
  for(team in id_map$team_names){
    if(team %in% NL){
      team_records = records %>%
        filter(Team == team) %>%
        summarize(AvgWins = mean(Wins),
                  MedianWins = median(Wins),
                  TenthPtileWins = round(quantile(Wins, 0.1)),
                  NinetiethPtileWins = round(quantile(Wins,0.9)),
                  MinWins = min(Wins),
                  MaxWins = max(Wins))
      
      playoff_odds = with(simmed_season_results, sum(team == NL_WC1_Winner) +
                            sum(team == NL_WC2_Winner) +
                            sum(team == NL_East_Winner) +
                            sum(team == NL_Central_Winner) +
                            sum(team == NL_West_Winner)) / sims
      division_odds = with(simmed_season_results, case_when(team %in% NL_East ~ sum(team == NL_East_Winner),
                                                            team %in% NL_Central ~ sum(team == NL_Central_Winner),
                                                            team %in% NL_West ~ sum(team == NL_West_Winner),
                                                            T~NA_integer_)) / sims
      wcg_odds = with(simmed_season_results, sum(team == NL_WC1_Winner) +
                        sum(team == NL_WC2_Winner)) / sims
      wc_odds = with(simmed_season_results, sum(team == NL_WC_Winner)) / sims
      ds_odds = with(simmed_season_results, sum(team == NLDS1_Winner) + sum(team == NLDS2_Winner)) / sims
      pennant_odds = with(simmed_season_results, sum(team == NLCS_Winner)) / sims
      ws_odds = with(simmed_season_results, sum(team == Winner)) / sims
      top_pick_odds = with(simmed_season_results, sum(team == Top_Pick)) / sims
      second_pick_odds = with(simmed_season_results, sum(team == Second_Pick)) / sims
      third_pick_odds = with(simmed_season_results, sum(team == Third_Pick)) / sims
      fourth_pick_odds = with(simmed_season_results, sum(team == Fourth_Pick)) / sims
      fifth_pick_odds = with(simmed_season_results, sum(team == Fifth_Pick)) / sims
      top_five_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick)) / sims
      top_ten_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                       team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick)) / sims
      top_fifteen_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                           team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick | 
                                                           team == Eleventh_Pick | team == Twelfth_Pick | team == Thirteenth_Pick | team == Fourteenth_Pick | team == Fifteenth_Pick)) / sims
      
      # RoS WPCT
      
      RoS = schedule_df %>%
        filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
        summarize(RoSG = n(),
                  RoSxW = sum(case_when(team == HomeTeam~HomeWpct,
                                        T~1-HomeWpct))) %>%
        mutate(RoSxWPct = round(RoSxW / RoSG,3))
      
      # SoS
      SoS = schedule_df %>%
        filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
        mutate(EloHome = case_when(HomeTeam == team~1500,
                                   T~EloHome),
               EloAway = case_when(AwayTeam == team~1500,
                                   T~EloAway),
               HomeWpct = 1 / (1 + 10 ^ ((EloAway - (EloHome + 25)) / 400))) %>%
        summarize(RoSG = n(),
                  NeutxW = sum(case_when(team == HomeTeam~HomeWpct,
                                         T~1-HomeWpct))) %>%
        mutate(SoS = round(NeutxW / RoSG,3))
      
      odds_df = odds_df %>%
        bind_rows(
          data.frame(
            team,
            playoff_odds,
            division_odds,
            wcg_odds,
            wc_odds,
            ds_odds,
            pennant_odds,
            ws_odds,
            top_pick_odds,
            second_pick_odds,
            third_pick_odds,
            fourth_pick_odds,
            fifth_pick_odds,
            top_five_odds,
            top_ten_odds,
            top_fifteen_odds,
            RoSG = RoS$RoSG,
            RoSxWPct = RoS$RoSxWPct,
            SoS = SoS$SoS
          ) %>% cbind(team_records)
        )
      
    } else {
      team_records = records %>%
        filter(Team == team) %>%
        summarize(AvgWins = mean(Wins),
                  MedianWins = median(Wins),
                  TenthPtileWins = round(quantile(Wins, 0.1)),
                  NinetiethPtileWins = round(quantile(Wins,0.9)),
                  MinWins = min(Wins),
                  MaxWins = max(Wins))
      
      playoff_odds = with(simmed_season_results, sum(team == AL_WC1_Winner) +
                            sum(team == AL_WC2_Winner) +
                            sum(team == AL_East_Winner) +
                            sum(team == AL_Central_Winner) +
                            sum(team == AL_West_Winner)) / sims
      division_odds = with(simmed_season_results, case_when(team %in% AL_East ~ sum(team == AL_East_Winner),
                                                            team %in% AL_Central ~ sum(team == AL_Central_Winner),
                                                            team %in% AL_West ~ sum(team == AL_West_Winner),
                                                            T~NA_integer_)) / sims
      wcg_odds = with(simmed_season_results, sum(team == AL_WC1_Winner) +
                        sum(team == AL_WC2_Winner)) / sims
      wc_odds = with(simmed_season_results, sum(team == AL_WC_Winner)) / sims
      ds_odds = with(simmed_season_results, sum(team == ALDS1_Winner) + sum(team == ALDS2_Winner)) / sims
      pennant_odds = with(simmed_season_results, sum(team == ALCS_Winner)) / sims
      ws_odds = with(simmed_season_results, sum(team == Winner)) / sims
      top_pick_odds = with(simmed_season_results, sum(team == Top_Pick)) / sims
      second_pick_odds = with(simmed_season_results, sum(team == Second_Pick)) / sims
      third_pick_odds = with(simmed_season_results, sum(team == Third_Pick)) / sims
      fourth_pick_odds = with(simmed_season_results, sum(team == Fourth_Pick)) / sims
      fifth_pick_odds = with(simmed_season_results, sum(team == Fifth_Pick)) / sims
      top_five_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick)) / sims
      top_ten_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                       team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick)) / sims
      top_fifteen_odds = with(simmed_season_results, sum(team == Top_Pick | team == Second_Pick | team == Third_Pick | team == Fourth_Pick | team == Fifth_Pick | 
                                                           team == Sixth_Pick | team == Seventh_Pick | team == Eighth_Pick | team == Ninth_Pick | team == Tenth_Pick | 
                                                           team == Eleventh_Pick | team == Twelfth_Pick | team == Thirteenth_Pick | team == Fourteenth_Pick | team == Fifteenth_Pick)) / sims
      
      # RoS WPCT
      
      RoS = schedule_df %>%
        filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
        summarize(RoSG = n(),
                  RoSxW = sum(case_when(team == HomeTeam~HomeWpct,
                                        T~1-HomeWpct))) %>%
        mutate(RoSxWPct = round(RoSxW / RoSG,3))
      
      # SoS
      SoS = schedule_df %>%
        filter(!FinishedGameFlg & (HomeTeam == team | AwayTeam == team)) %>%
        mutate(EloHome = case_when(HomeTeam == team~1500,
                                   T~EloHome),
               EloAway = case_when(AwayTeam == team~1500,
                                   T~EloAway),
               HomeWpct = 1 / (1 + 10 ^ ((EloAway - (EloHome + 25)) / 400))) %>%
        summarize(RoSG = n(),
                  NeutxW = sum(case_when(team == HomeTeam~HomeWpct,
                                         T~1-HomeWpct))) %>%
        mutate(SoS = round(NeutxW / RoSG,3))
      
      odds_df = odds_df %>%
        bind_rows(
          data.frame(
            team,
            playoff_odds,
            division_odds,
            wcg_odds,
            wc_odds,
            ds_odds,
            pennant_odds,
            ws_odds,
            top_pick_odds,
            second_pick_odds,
            third_pick_odds,
            fourth_pick_odds,
            fifth_pick_odds,
            top_five_odds,
            top_ten_odds,
            top_fifteen_odds,
            RoSG = RoS$RoSG,
            RoSxWPct = RoS$RoSxWPct,
            SoS = SoS$SoS
          ) %>% cbind(team_records)
        )
    }
  }
  
  # odds_df$Date = date
  odds_df$Week = week
  
  odds_over_time = bind_rows(odds_over_time,odds_df)
}

write.csv(odds_over_time,'odds_over_time.csv')
write.csv(odds_df %>% arrange(-ws_odds),'curr_odds.csv')
write.csv(elo_df %>% select(-c(`#`)),'elo_df.csv')
write.csv(simmed_season_results,'all_sims.csv')
write.csv(records,'all_sim_records.csv')

# # ss1 <- gs4_create(
# #   "playoff_odds",
# #   sheets = c("Playoff Odds",
# #              "Elo Ratings",
# #              "Schedule and Odds",
# #              "All Sims Results",
# #              "All Sim Records")
# # )
# 
# ss1 = '1OgIsVihGelHkwgyO8cJQdpor0KyBuJVFYAEa00_gDKE'
# 
# sheet_write(odds_df %>% arrange(-ws_odds), ss = ss1, sheet = "Playoff Odds")
# sheet_write(elo_df %>% select(-c(`#`)), ss = ss1, sheet = "Elo Ratings")
# sheet_write(schedule_df, ss = ss1, sheet = "Schedule and Odds")
# sheet_write(simmed_season_results, ss = ss1, sheet = "All Sims Results")
# sheet_write(records, ss = ss1, sheet = "All Sim Records")