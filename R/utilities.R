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
    league_link <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID,'?season=',season)
    scores_link <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/scores",'?season=',season)
    players_link <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/players",'?season=',season)
}

#' Get teams from main league page
#'
#' @param link Link to main league page
#' @return Data frame with team ID and team name
#' @examples
#' getTeams(123456)
#' @export
getTeams <- function(league_link) {
    # Get HTML
    teams <- xml2::read_html(league_link)
    team_owners <- teams %>% rvest::html_nodes(".user-name") %>% rvest::html_text()
    team_names <- teams %>% rvest::html_nodes(".league-name") %>% rvest::html_text()
    team_owners <- team_owners[1:length(team_names)]
    team_links <- teams %>% rvest::html_nodes(".league-name a") %>% rvest::html_attr("href")
    team_ids <- teams %>% rvest::html_nodes(".league-name a") %>% rvest::html_attr("href") %>% stringr::str_extract("\\d+$")
    return(data.frame(teamOwner = team_owners, teamID = team_ids, teamLink = paste0('https://www.fleaflicker.com/',team_links), teamName = team_names))
}

getRosters <- function(teams, week) {
  rosters <- data.frame(week = numeric(), teamID = numeric(), playerLink = character(), playerName = character(), starter = character())
  for(i in 1:nrow(teams)){
    teamID <- teams[i, "teamID"]
    teamLink <- as.character(teams[i, "teamLink"])
    roster <- xml2::read_html(paste0(teamLink,'?week=',week))
    player_tr <- roster %>% rvest::html_nodes("tr")
    player_tr <- player_tr[c(3:12,15:length(player_tr))]
    for(j in 1:length(player_tr)){
      playerNames <- player_tr[j] %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
      if(length(playerNames)){
        playerLinks <- paste0('https://www.fleaflicker.com/',player_tr[j] %>% rvest::html_nodes(".player-text") %>% rvest::html_attr("href"))
        rosters <- rbind(rosters, data.frame(week = week, teamID = teamID, playerLink = playerLinks, playerName = playerNames, starter = ifelse(j <= 10,"yes","no")))
      }
    }
  }
  return(rosters)
}
