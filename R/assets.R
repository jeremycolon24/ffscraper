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
  rosters <- data.frame(season = numeric(), week = numeric(), teamID = numeric(), playerLink = character(), playerName = character(), starter = character())
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
        rosters <- rbind(rosters, data.frame(season = season, week = week, teamID = teamID, playerLink = playerLinks, playerName = playerNames, starter = ifelse(j <= 10,"yes","no")))
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
