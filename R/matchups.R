# Matchups
#
# A set of functions that pull matchup data
#

#' Gets all transactions from transaction log
#'
#' @param matchupsLink Web link to transaction log
#' @param week Matchup week
#' @return Matchups and Scores
#' @examples
#' getMatchups(matchupsLink, 1)
#' @export
#'
getMatchups <- function(matchupsLink, season, week) {
  matchupsLinkParam <- paste0(matchupsLink,"&week=",week)
  matchups <- xml2::read_html(matchupsLinkParam)

  pointsType <- (matchups %>% rvest::html_nodes(".leaf") %>% rvest::html_text())[2]
  if(pointsType == "Projected"){
    stop("Can't return scores for future weeks")
  }

  teams <- matchups %>% rvest::html_nodes(".table-group .league-name") %>% rvest::html_text()
  links <- matchups %>% rvest::html_nodes(".table-group a") %>% rvest::html_attr("href")
  teamIDs <- links[grepl('teams',links)] %>% stringr::str_remove("[?]season=\\d+$") %>% stringr::str_extract('\\d+$')
  scoreLinks <- links[grepl('scores',links)] %>% stringr::str_remove("/plays$")
  points <- matchups %>% rvest::html_nodes(".table-group .text-right") %>% rvest::html_text() %>% stringr::str_remove_all("Points")
  points <- points[points != ""]
  scores <- data.frame(season = season, week = week, teamName = teams, teamID = teamIDs, scoreLink = scoreLinks, points = as.numeric(points))
  scores['idx'] <- as.integer(row.names(scores))
  scores <- scores %>% filter(idx%%2 == 0) %>%
    inner_join(scores %>% filter(idx%%2 == 1), by = "scoreLink") %>%
    mutate(winningTeam = dplyr::if_else(points.x > points.y, teamID.x, teamID.y),
           losingTeam = dplyr::if_else(points.x > points.y, teamID.y, teamID.x),
           winningPoints = dplyr::if_else(points.x > points.y, points.x, points.y),
           losingPoints = dplyr::if_else(points.x > points.y, points.y, points.x),
           scoreLink = paste0("https://www.fleaflicker.com",scoreLink),
           season = season) %>%
    select(season, week.x, winningTeam, winningPoints, losingTeam, losingPoints, scoreLink) %>%
    dplyr::rename(week = week.x)

  return(scores)

}
