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
#' getAllPlayerPoints(playersLink, season, week, 2, 2, FALSE, "ALL")
#' @export
getAllPlayerPoints <- function(playersLink, season, week, statType = 2, sortType = 2, FA_only = FALSE, position = "ALL") {
    position <- stringr::str_to_upper(position)
    positions <- data.frame(id = c(4, 1, 2, 8, 11, 16, 256), pos = c("QB", "RB", "WR", "TE", "FLEX", "K", "D/ST"))
    if (!position %in% positions$pos & position != "ALL") {
        stop("Not a valid position. Please choose from the following: 'QB','RB','WR','TE','FLEX','K','D/ST','ALL'")
    }
    if (position == "ALL") {
        positions <- positions %>% dplyr::filter(pos != "FLEX") %>% dplyr::mutate(link = paste0(playersLink, "&season=", season, "&week=", week, "&statType=", statType, "&sortMode=", sortType, "&position=", id, "&isFreeAgent=",
            ifelse(FA_only, "true", "false")))
    } else {
        positions <- positions %>% dplyr::filter(pos == position) %>% dplyr::mutate(link = paste0(playersLink, "&season=", season, "&week=", week, "&statType=", statType, "&sortMode=", sortType, "&position=", id, "&isFreeAgent=",
            ifelse(FA_only, "true", "false")))
    }

    playerData <- data.frame(season = numeric(), week = numeric(), id = integer(), name = character(),
                             position = character(), team = character(), points = numeric(),
                             opponent = character(), gameHomeAway = character(), gameWinLoss = character(),
                             gameWinScore = integer(), gameLossScore = integer())
    page_offset <- 0
    for (i in 1:nrow(positions)) {
        players <- xml2::read_html(paste0(as.character(positions[i, "link"]), "&tableOffset=", page_offset))
        playerNames <- players %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
        while (length(playerNames) > 0) {
            playerIDs <- players %>% rvest::html_nodes(".player-text") %>% rvest::html_attr('href') %>% as.character() %>% stringr::str_remove('[?]season=\\d+$') %>% stringr::str_extract('\\d+$') %>% as.integer()
            playerOpp <- players %>% rvest::html_nodes("div.pro-opp-matchup") %>% rvest::html_nodes("a") %>% rvest::html_text()
            playerWinLoss <- playerOpp %>% as.character() %>% stringr::str_extract('^[@]?[A-Z]{3,4}') %>% stringr::str_extract('W$|L$')
            playerWinLossScore <- playerOpp %>% as.character() %>% stringr::str_extract('\\d+-\\d+[ ]?$') %>% stringr::str_trim()
            playerScore1 <- playerWinLossScore %>% stringr::str_extract('^\\d{1,2}') %>% as.integer()
            playerScore2 <- playerWinLossScore %>% stringr::str_extract('\\d{1,2}$') %>% as.integer()
            playerWinScore <- pmax(playerScore1, playerScore2)
            playerLossScore <- pmin(playerScore1, playerScore2)
            playerHomeAway <- playerOpp %>% as.character() %>% stringr::str_extract('^[@]?')
            playerOpp <- playerOpp %>% as.character() %>% stringr::str_extract('^[@]?[A-Z]{3,4}') %>% stringr::str_remove('W$|L$') %>% stringr::str_remove('^[@]')
            playerPos <- players %>% rvest::html_nodes(".position") %>% rvest::html_text()
            playerTeam <- players %>% rvest::html_nodes(".player-team") %>% rvest::html_text()
            playerPoints <- players %>% rvest::html_nodes(".points-final") %>% rvest::html_text()
            playerData <- rbind(playerData, data.frame(season, week, id = playerIDs, name = playerNames, position = playerPos,
                                                       team = playerTeam, points = playerPoints, opponent = playerOpp,
                                                       gameHomeAway = playerHomeAway, gameWinLoss = playerWinLoss,
                                                       gameWinScore = playerWinScore, gameLossScore = playerLossScore))
            page_offset <- page_offset + 20
            players <- xml2::read_html(paste0(as.character(positions[i, "link"]), "&tableOffset=", page_offset))
            playerNames <- players %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
        }
        page_offset <- 0
    }
    return(playerData)
}
