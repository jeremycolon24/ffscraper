# Utilities
#
# A set of functions that get the necessary data to pull other data.
#     from fleaflicker
#

#' Get links from Fleaflicker website
#'
#' @param leagueID Your Fleaflicker league ID
#' @return Links added to your environment
#' @examples
#' getLinks(123456)
#' @export
getLinks <- function(leagueID, season) {
    leagueLink <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID,'?season=',season)
    scoresLink <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/scores?season=",season)
    playersLink <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/players?season=",season)
    transLogLink <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/transactions")
    tradesLink <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/trades")
    matchupsLink <<- paste0("https://www.fleaflicker.com/nfl/leagues/",leagueID, "/scores?season=",season)
}

#' Get teams from main league page
#'
#' @param link Link to main league page
#' @return Data frame with team ID and team name
#' @examples
#' getTeams(123456)
#' @export
getTeams <- function(leagueLink) {
    # Get HTML
    teams <- xml2::read_html(leagueLink)
    team_owners <- teams %>% rvest::html_nodes(".user-name") %>% rvest::html_text()
    team_names <- teams %>% rvest::html_nodes(".league-name") %>% rvest::html_text()
    team_owners <- team_owners[1:length(team_names)]
    team_links <- teams %>% rvest::html_nodes(".league-name a") %>% rvest::html_attr("href")
    team_ids <- teams %>% rvest::html_nodes(".league-name a") %>% rvest::html_attr("href") %>% stringr::str_extract("teams[/]\\d+") %>% stringr::str_extract("\\d+") %>% as.integer()
    season <- team_links %>% stringr::str_extract('\\d+$') %>% as.integer()
    if(season[1] == team_ids[1]) {
      season <- rep(teams %>% rvest::html_nodes(".dropdown-toggle") %>% rvest::html_text() %>% trimws(),length(team_ids))
    }
    return(data.frame(season = season, teamOwner = team_owners, teamID = team_ids, teamLink = paste0('https://www.fleaflicker.com',team_links), teamName = team_names))
}

getRosters <- function(teams, season, week, current = 0) {
  rosters <- data.frame(week = numeric(), teamID = numeric(), playerLink = character(), playerName = character(), starter = character())
  for(i in 1:nrow(teams)){
    teamID <- teams[i, "teamID"]
    teamLink <- as.character(teams[i, "teamLink"])
    if(current == 0){
      teamLink <- if_else(grepl('season',teamLink),paste0(teamLink,'&week=',week),paste0(teamLink,'?week=',week))
    }
    roster <- xml2::read_html(teamLink)
    player_tr <- roster %>% rvest::html_nodes("tr")
    player_tr <- player_tr[c(3:12,15:length(player_tr))]
    for(j in 1:length(player_tr)){
      playerNames <- player_tr[j] %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
      if(length(playerNames)){
        playerLinks <- paste0('https://www.fleaflicker.com',player_tr[j] %>% rvest::html_nodes(".player-text") %>% rvest::html_attr("href"))
        rosters <- rbind(rosters, data.frame(week = week, teamID = teamID, playerLink = playerLinks, playerName = playerNames, starter = ifelse(j <= 10,"yes","no")))
      }
    }
  }
  return(rosters %>% mutate(season = season))
}

getDraftPicks <- function(teams) {
  picks_df <- data.frame(draftSeason = numeric(), teamID = numeric(), roundPick = character(), overallPick = numeric(), origin_destination = character(), to_from_team = character(), trade_link = character())
  for(i in 1:nrow(teams)){
    teamID <- teams[i, "teamID"]
    picksLink <- paste0(as.character(teams[i, "teamLink"]),"/picks")
    picks <- xml2::read_html(picksLink)
    pick_tr <- picks %>% rvest::html_nodes("table") %>% rvest::html_nodes("tr")
    for(j in 1:length(pick_tr)){
      pickSeason <- pick_tr[j] %>% rvest::html_nodes("h3")
      if(length(pickSeason)){
        draftSeason <- pickSeason %>% rvest::html_text() %>% stringr::str_remove(" Draft Picks") %>% as.numeric()
      }
      pickData <- pick_tr[j] %>% rvest::html_nodes("td .text-muted")
      if(length(pickData)){
        pickData <- pick_tr[j]
        roundPick <- (pickData %>% rvest::html_nodes("td") %>% rvest::html_text())[2] %>% stringr::str_remove(' #\\d+$')
        overallPick <- pickData %>% rvest::html_nodes("td .text-muted") %>% rvest::html_text() %>% stringr::str_remove('#') %>% as.numeric()
        status <-  pickData %>% rvest::html_nodes("td a") %>% stringr::str_extract('Obtained|Traded') %>% na.omit() %>% as.character()
        status <- if(length(status)) {status} else {"Own"}
        to_from_team <- pickData %>% rvest::html_nodes("td div a") %>% rvest::html_text()
        if(length(to_from_team)) {
          if(stringr::str_detect(to_from_team,"[.]+$")){
            to_from_team <- pickData %>% rvest::html_nodes("td a") %>% rvest::html_attr("title") %>% na.omit() %>% as.character()
          }
        } else {
          to_from_team <- "N/A"
        }
        trade_link <- pickData %>% rvest::html_nodes("td a") %>% rvest::html_attr("href") %>% stringr::str_extract(".*trades.*") %>% na.omit() %>% as.character()
        trade_link <- if(length(trade_link)) {paste0('https://www.fleaflicker.com',trade_link)} else {"N/A"}
        picks_df <- rbind(picks_df, data.frame(draftSeason = draftSeason, teamID = teamID, roundPick = roundPick, overallPick = overallPick, status = status, to_from_team = to_from_team, trade_link = trade_link))
      }
    }
  }
  return(picks_df)
}
