# Utilities
#
# A set of functions that get the necessary data to pull other data from fleaflicker.
#

#' Gets links from Fleaflicker website and adds them to your local variables
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

#' Gets team information from main league page
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
