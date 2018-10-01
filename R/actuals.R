# Actuals
#
# A set of functions that pull actual points
#

#' Get All Player Fantasy Points
#'
#' @param teams Data frame of teams from getTeams function
#' @param week Week number
#' @return Each team's roster with fantasy points and starter designation.
#' @examples
#' getPlayerPoints(teams, 1)
#' @export
getAllPlayerPoints <- function(playersLink, week, statType = 2, sortType = 2, FA_only = TRUE, position = "ALL") {
    position <- stringr::str_to_upper(position)
    positions <- data.frame(id = c(4, 1, 2, 8, 11, 16, 256), pos = c("QB", "RB", "WR", "TE", "FLEX", "K", "D/ST"))
    if (!position %in% positions$pos & position != "ALL") {
        stop("Not a valid position. Please choose from the following: 'QB','RB','WR','TE','FLEX','K','D/ST','ALL'")
    }
    if (position == "ALL") {
        positions <- positions %>% dplyr::filter(pos != "FLEX") %>% dplyr::mutate(link = paste0(playersLink, "&week=", week, "&statType=", statType, "&sortMode=", sortType, "&position=", id, "&isFreeAgent=",
            ifelse(FA_only, "true", "false")))
    } else {
        positions <- positions %>% dplyr::filter(pos == position) %>% dplyr::mutate(link = paste0(playersLink, "&week=", week, "&statType=", statType, "&sortMode=", sortType, "&position=", id, "&isFreeAgent=",
            ifelse(FA_only, "true", "false")))
    }

    playerData <- data.frame(season = numeric(), week = numeric(), name = character(), position = character(), team = character(), points = numeric())
    season <- playersLink %>% stringr::str_extract('\\d+$') %>% as.integer()
    page_offset <- 0
    for (i in 1:nrow(positions)) {
        players <- xml2::read_html(paste0(as.character(positions[i, "link"]), "&tableOffset=", page_offset))
        playerNames <- players %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
        while (length(playerNames) > 0) {
            playerPos <- players %>% rvest::html_nodes(".position") %>% rvest::html_text()
            playerTeam <- players %>% rvest::html_nodes(".player-team") %>% rvest::html_text()
            playerPoints <- players %>% rvest::html_nodes(".points-final") %>% rvest::html_text()
            playerData <- rbind(playerData, data.frame(season, week, name = playerNames, position = playerPos, team = playerTeam, points = playerPoints))
            page_offset <- page_offset + 20
            players <- xml2::read_html(paste0(as.character(positions[i, "link"]), "&tableOffset=", page_offset))
            playerNames <- players %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
        }
        page_offset <- 0
    }
    return(playerData)
}
