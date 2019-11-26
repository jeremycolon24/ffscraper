# Personnel & Assets
#
# A set of functions that pulls rosters & draft picks
#

#' Get rosters for each team
#'
#' @param teams teams data frame. You can use the getTeams function for this.
#' @param season Season to get rosters for
#' @param week Week to get rosters for within season. Useful if you want to see a past lineup.
#' @param current Default value = 0. Set to 1 disregard season & week params to get the current set roster.
#' @return Data frame with team ID and team name
#' @examples
#' getRosters(teams, 2019, 12)
#' getRosters(teams, 2019, 1, 1) # You can put any season/week value if current == 1.
getRosters <- function(teams, season, week, current = 0) {
  rosters <- data.frame(season = numeric(), week = numeric(), teamID = numeric(), playerLink = character(), playerName = character(), positionSlot = character(), starter = character())
  for(i in 1:nrow(teams)){
    teamID <- teams[i, "teamID"]
    teamLink <- as.character(teams[i, "teamLink"])
    if(current == 0){
      teamLink <- if_else(grepl('season',teamLink),paste0(teamLink,'&week=',week),paste0(teamLink,'?season=',season,'&week=',week))
    }
    roster <- xml2::read_html(teamLink)
    player_tr <- roster %>% rvest::html_nodes("tr")
    player_tr <- player_tr[c(3:12,15:length(player_tr))]
    for(j in 1:length(player_tr)){
      playerNames <- player_tr[j] %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
      if(length(playerNames)){
        playerLinks <- paste0('https://www.fleaflicker.com',player_tr[j] %>% rvest::html_nodes(".player-text") %>% rvest::html_attr("href"))
        playerPosSlot <- player_tr[j] %>% rvest::html_nodes(".label.label-success.label-block") %>% rvest::html_text()
        if(length(playerPosSlot) == 0){
          playerPosSlot <- 'Bench'
        }
        rosters <- rbind(rosters, data.frame(season = season, week = week, teamID = teamID, playerLink = playerLinks, playerName = playerNames, positionSlot = playerPosSlot, starter = ifelse(j <= 10,"yes","no")))
      }
    }
  }
  return(rosters)
}

#' Get all draft picks currently visible on each teams picks page
#'
#' @param teams teams data frame. You can use the getTeams function for this.
#' @examples
#' getDraftPicks(teams)
getDraftPicks <- function(teams) {
  picks_df <- data.frame(draftSeason = numeric(), teamID = numeric(), roundPick = character(), overallPick = numeric(), origin_destination = character(), to_from_team = character(), trade_link = character(), pick_used = character())
  for(i in 1:nrow(teams)){
    teamID <- teams[i, "teamID"]
    picksLink <- paste0(as.character(teams[i, "teamLink"]),"/picks")
    picks <- xml2::read_html(picksLink)
    pick_tr <- picks %>% rvest::html_nodes("table") %>% rvest::html_nodes("tr")
    for(j in 1:length(pick_tr)){
      pickSeason <- pick_tr[j] %>% rvest::html_nodes("h3")
      if(length(pickSeason) > 0){
        draftSeason <- pickSeason %>% rvest::html_text() %>% stringr::str_remove(" Draft Picks") %>% as.numeric()
      }
      pickUsed <- pick_tr[j] %>% rvest::html_nodes("em")
      if(length(pickUsed) > 0){
        pick_used <- pickUsed %>% rvest::html_text()
      } else {
        pick_used <- "Used"
      }
      pickData <- pick_tr[j] %>% rvest::html_nodes("td .text-muted")
      if(length(pickData) > 0){
        pickData <- pick_tr[j]
        roundPick <- (pickData %>% rvest::html_nodes("td") %>% rvest::html_text())[2] %>% stringr::str_remove(' #\\d+$')
        overallPick <- pickData %>% rvest::html_nodes("td .text-muted") %>% rvest::html_text() %>% stringr::str_remove('#') %>% as.numeric()
        status <-  pickData %>% rvest::html_nodes("td a") %>% as.character() %>% stringr::str_extract('Obtained|Traded') %>% na.omit() %>% as.character()
        status <- if(length(status) > 0) {status} else {"Own"}
        to_from_team <- pickData %>% rvest::html_nodes("td div a") %>% rvest::html_text()
        if(length(to_from_team) > 0) {
          if(stringr::str_detect(to_from_team,"[.]+$")){
            to_from_team <- pickData %>% rvest::html_nodes("td a") %>% rvest::html_attr("title") %>% na.omit() %>% as.character()
          }
        } else {
          to_from_team <- "N/A"
        }
        trade_link <- pickData %>% rvest::html_nodes("td a") %>% rvest::html_attr("href") %>% stringr::str_extract(".*trades.*") %>% na.omit() %>% as.character()
        trade_link <- if(length(trade_link) > 0) {paste0('https://www.fleaflicker.com',trade_link)} else {"N/A"}
        picks_df <- rbind(picks_df, data.frame(draftSeason = draftSeason, teamID = teamID, roundPick = roundPick, overallPick = overallPick, status = status, to_from_team = to_from_team, trade_link = trade_link, pick_used = pick_used))
      }
    }
  }
  return(picks_df)
}


#' Get all players from Players page
#'
#' @param playerLink link to players page
#' @param leagueID ID of league
#' @param fa_only free agents only
#' @param rookies_only rookies only
#' @param offseason in-season or offseason flag
#' @examples
#' getPlayers(playerLink, fa_only = TRUE, rookies_only = FALSE, offseason = FALSE)
getPlayers <- function(playerLink, leagueID, fa_only = FALSE, rookies_only = FALSE, offseason = FALSE){
  offset <- 0
  playerLink <- paste0(playerLink,"?statType=0&sortMode=0&position=287&tableOffset=",offset)
  if(rookies_only){
    playerLink <- paste0(playerLink,'&rookies=true')
  }
  if(!fa_only){
    playerLink <- paste0(playerLink,'&isFreeAgent=false')
  }

  players <- xml2::read_html(playerLink)
  player_info <- players %>% rvest::html_nodes("td") %>% rvest::html_text()
  player_ids <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_extract(stringr::str_c('href=\"/nfl/leagues/',as.character(leagueID),'/players/.*?-\\d+"')) %>% stringr::str_extract('-\\d+..') %>% stringr::str_extract('\\d+') %>% as.integer()
  teams <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_extract('<a href=\"/nfl/leagues/197269/teams/\\d+') %>% stringr::str_extract('\\d+$') %>% as.integer()
  rookie_flag <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_detect('graduation-cap')
  injury_flag <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_extract('span class="injury.*[>][A-Z]+[<][/]span[>][<]a class="player-text"') %>% stringr::str_extract('>[A-Z]+<') %>% stringr::str_extract('[A-Z]+')
  player_df <- as.data.frame(matrix(0, ncol = 19, nrow = 0))
  player_df <- player_df %>% mutate_all(as.character)

  if(offseason){
    seq_end <- 20
  } else {
    seq_end <- 19
  }

  while(length(player_info) > 0){
    for(i in seq(1,seq_end+1)){
      player_df = rbind(player_df,
                        t(c(player_info[c((seq(1,(seq_end-2))+((seq_end-2)*(i-1))))],
                            player_ids[(i*(seq_end-2))-(seq_end-3)],
                            teams[(i*(seq_end-2)) - 2],
                            rookie_flag[(i*(seq_end-2))-(seq_end-3)],
                            injury_flag[(i*(seq_end-2))-(seq_end-3)])))
    }
    offset = offset + 20
    playerLink <- paste0("https://www.fleaflicker.com/nfl/leagues/197269/players?statType=0&sortMode=0&position=287&tableOffset=",offset)
    if(rookies_only){
      playerLink <- paste0(playerLink,'&rookies=true')
    }
    if(!fa_only){
      playerLink <- paste0(playerLink,'&isFreeAgent=false')
    }
    players <- xml2::read_html(playerLink)
    player_info2 <- players %>% rvest::html_nodes("td") %>% rvest::html_text()
    if(identical(player_info,player_info2)){
      break
    } else {
      player_info <- player_info2
      player_ids <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_extract(stringr::str_c('href=\"/nfl/leagues/',as.character(leagueID),'/players/.*?-\\d+"')) %>% stringr::str_extract('-\\d+..') %>% stringr::str_extract('\\d+') %>% as.integer()
      rookie_flag <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_detect('graduation-cap')
      injury_flag <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_extract('span class="injury.*[>][A-Z]+[<][/]span[>][<]a class="player-text"') %>% stringr::str_extract('>[A-Z]+<') %>% stringr::str_extract('[A-Z]+')
      teams <- players %>% rvest::html_nodes("td") %>% as.character() %>% stringr::str_extract('<a href=\"/nfl/leagues/197269/teams/\\d+') %>% stringr::str_extract('\\d+$') %>% as.integer()
    }
  }

  if(offseason){
    colnames(player_df) <- c('name','null1','next_game','null3',
                           'null4','pos_draft_rank','null5','pos_fantasy_rank',
                           'null6','last_1','last_3','last_5','total','avg',
                           'null7','null8','pct_owned','action','rookie','injury')
    player_df <- player_df[,!stringr::str_detect(colnames(player_df),'null')]
      player_df <- player_df %>%
    mutate(last_1 = as.numeric(stringr::str_extract(last_1,'[0-9]+[.][0-9]+')),
           last_3 = as.numeric(stringr::str_extract(last_3,'[0-9]+[.][0-9]+')),
           last_5 = as.numeric(stringr::str_extract(last_5,'[0-9]+[.][0-9]+')),
           total = as.numeric(stringr::str_extract(total,'[0-9]+[.][0-9]+')),
           avg = as.numeric(stringr::str_extract(avg,'[0-9]+[.][0-9]+')),
           pct_owned = as.numeric(stringr::str_remove(pct_owned,'%')),
           pos_fantasy_rank = if_else(pos_fantasy_rank == '', 'N/A',as.character(pos_fantasy_rank)),
           position = stringr::str_extract(pos_draft_rank,'^[A-Z/]+'),
           pos_draft_rank = stringr::str_remove(pos_draft_rank,position),
           location = if_else(stringr::str_detect(next_game,'@'),'Away','Home'),
           next_opponent = stringr::str_remove(stringr::str_remove(stringr::str_extract(next_game, '[@]?[A-Z]{2,3}[A-Z][a-z]{2} '),'[A-Z][a-z]{2} '),'@'),
           team = stringr::str_remove(stringr::str_remove(stringr::str_extract(name, paste0(position,' [A-Z]{2,3} [0-9()]+')), paste0(position,' ')),' [0-9()]+'),
           name = stringr::str_remove(name, paste0(position,' [A-Z]{2,3} [0-9()]+')),
           name = if_else(is.na(injury), name, stringr::str_remove(name, paste0('^',injury))),
           rookie = if_else(as.character(rookie) == "TRUE",'Rookie','Veteran')
           ) %>%
    select(name, position, team, experience = rookie, injury, pos_draft_rank, pos_fantasy_rank, pct_owned, next_opponent, location, last_1, last_3, last_5, total, avg)
  } else {
    colnames(player_df) <- c('name','null1','next_game','projection',
                           'null4','null5','pos_fantasy_rank','null6',
                           'last_1','last_3','last_5','total','avg',
                           'null7', 'owner','pct_owned','action', 'playerID',
                           'teamID','rookie','injury')
    player_df <- player_df[,!stringr::str_detect(colnames(player_df),'null')]
    player_df <- player_df %>%
      mutate(last_1 = as.numeric(stringr::str_extract(last_1,'[0-9.]+')),
             last_3 = as.numeric(stringr::str_extract(last_3,'[0-9.]+')),
             last_5 = as.numeric(stringr::str_extract(last_5,'[0-9.]+')),
             total = as.numeric(stringr::str_extract(total,'[0-9.]+')),
             avg = as.numeric(stringr::str_extract(avg,'[0-9.]+')),
             owner = if_else(owner == '', 'Free Agent', as.character(owner)),
             pct_owned = as.numeric(stringr::str_remove(pct_owned,'%')),
             pos_fantasy_rank = as.numeric(as.character(pos_fantasy_rank)),
             position = stringr::str_extract(stringr::str_extract(name,'[A-Z/]+ [A-Z]{2,3} [(]\\d+[)]$|[A-Z/]+ FA$'), '[A-Z/]+'),
             location = if_else(stringr::str_detect(next_game,'@'),'Away','Home'),
             next_opponent = stringr::str_remove(stringr::str_remove(stringr::str_extract(next_game, '[@]?[A-Z]{2,3}[A-Z][a-z]{2} '),'[A-Z][a-z]{2} '),'@'),
             playerID = as.integer(as.character(playerID)),
             team = stringr::str_remove(stringr::str_remove(stringr::str_extract(name, paste0(position,' [A-Z]{2,3} [(]\\d+[)]$')), paste0(position,' ')),' [0-9()]+'),
             name = stringr::str_remove(name, ' [A-Z/]+ [A-Z]{2,3} [(]\\d+[)]$'),
             name = if_else(is.na(injury), name, stringr::str_remove(name, paste0('^',injury))),
             rookie = if_else(as.character(rookie) == "TRUE",'Rookie','Veteran')
             ) %>%
      select(playerID, name, teamID, position, team, experience = rookie, injury, pos_fantasy_rank, pct_owned, next_opponent, location, last_1, last_3, last_5, total, avg)
  }

  player_df <- player_df %>% filter(!is.na(team))
  return(player_df)
}
