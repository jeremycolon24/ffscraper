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
getLinks <- function(leagueID) {
    league_link <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID)
    scores_link <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/scores")
    players_link <<- paste0("https://www.fleaflicker.com/nfl/leagues/", leagueID, "/players")
}

#' Get teams from main league page
#'
#' @param link Link to main league page
#' @return Data frame with team ID and team name
#' @examples
#' getTeams(123456)
#' @export
getTeams <- function(link) {
    # Get HTML
    teams <- xml2::read_html(link)
    team_names <- teams %>% rvest::html_nodes(".league-name") %>% rvest::html_text()
    team_ids <- teams %>% rvest::html_nodes(".league-name a") %>% rvest::html_attr("href") %>% stringr::str_extract("\\d+$")
    return(data.frame(teamID = team_ids, teamName = team_names))
}

