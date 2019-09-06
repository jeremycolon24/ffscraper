# Projections
#
# A set of functions that pull projections.
# Note: Projections can NOT be retrieved historically.
#     Fleaflicker does not keep that data.
#     Projections must be retrieved during the week that you want projections for
#

#' Get Projected Player Scores
#'
#' @param playersLink Web link to main league page
#' @param week Week Number
#' @param statType The statistic that is shown on the page - defaults to projection.
#' @param sortType The statistic to sort on - defaults to projection
#' @param FA_only Boolean variable that determines if only free agents are included
#' @param position Player position to get projections for - defaults to ALL
#' @return Teams and projected total score.
#' @examples
#' getPlayerProjections(playersLink, 1, 7, 7, TRUE, 'QB')
#' getPlayerProjections(playersLink, 1, 7, 7, FALSE, c('QB','K'))
#' @export
getPlayerProjections <- function(playersLink, season, week, statType = 7, sortType = 7, FA_only = TRUE, position = "ALL") {
    playersLink <- stringr::str_remove(playersLink,'[?]season=\\d+')
    position <- stringr::str_to_upper(position)
    positions <- data.frame(id = c(4, 1, 2, 8, 11, 16, 256), pos = c("QB", "RB", "WR", "TE", "FLEX", "K", "D/ST"))
    if (!position %in% positions$pos & position != "ALL") {
        stop("Not a valid position. Please choose from the following: 'QB','RB','WR','TE','FLEX','K','D/ST','ALL'")
    }
    if (position == "ALL") {
        positions <- positions %>% dplyr::filter(pos != "FLEX") %>% dplyr::mutate(link = paste0(playersLink, "?week=", week, "&statType=", statType, "&sortMode=", sortType, "&position=", id, "&isFreeAgent=",
            ifelse(FA_only, "true", "false")))
    } else {
        positions <- positions %>% dplyr::filter(pos == position) %>% dplyr::mutate(link = paste0(playersLink, "?week=", week, "&statType=", statType, "&sortMode=", sortType, "&position=", id, "&isFreeAgent=",
            ifelse(FA_only, "true", "false")))
    }

    playerData <- data.frame(name = character(), position = character(), team = character(), opponent = character(), gametime = character(), projection = numeric(), percent_owned = numeric(),
        injury = character())
    playerInjuries <- data.frame(name = character(), injury = character())
    page_offset <- 0
    for (i in 1:nrow(positions)) {
        players <- xml2::read_html(paste0(as.character(positions[i, "link"]), "&tableOffset=", page_offset))
        playerNames <- players %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
        while (length(playerNames) > 0) {
            players <- xml2::read_html(paste0(as.character(positions[i, "link"]), "&tableOffset=", page_offset))
            playerNames <- players %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
            playerPos <- players %>% rvest::html_nodes(".position") %>% rvest::html_text()
            playerTeam <- players %>% rvest::html_nodes(".player-team") %>% rvest::html_text()
            playerGameTime <- players %>% rvest::html_nodes(".pro-opp-matchup-info") %>% rvest::html_text()
            playerOpp <- players %>% rvest::html_nodes(".pro-opp-matchup") %>% rvest::html_text()
            playerOpp <- playerOpp[-1]
            playerOpp <- stringr::str_remove(playerOpp, playerGameTime)
            if(length(players %>% rvest::html_nodes(".points-projected-live") %>% rvest::html_text()) > 0) {
              playerProj <- players %>% rvest::html_nodes(".points-projected-live") %>% rvest::html_text()
            } else {
              playerProj <- players %>% rvest::html_nodes(".points-projected") %>% rvest::html_text()
            }
            playerPercOwn <- players %>% rvest::html_nodes(".progress-bar") %>% rvest::html_text()
            playerInjury <- players %>% rvest::html_nodes(".player-name") %>% rvest::html_children() %>% rvest::html_text()
            if (length(playerInjury) > 1) {
                for (j in 1:(length(playerInjury) - 1)) {
                  if (playerInjury[j] %in% c("Q", "D", "OUT", "IR", "SUS")) {
                    playerInjuries <- rbind(playerInjuries, data.frame(name = playerInjury[j + 1], injury = playerInjury[j]))
                  }
                }
            }
            playerData <- rbind(playerData, data.frame(name = playerNames, position = playerPos, team = playerTeam, opponent = playerOpp, gametime = playerGameTime, projection = playerProj,
                percent_owned = playerPercOwn))
            page_offset <- page_offset + 20
        }
        page_offset <- 0
    }
    playerData <- suppressWarnings(playerData %>% dplyr::left_join(playerInjuries, by = "name") %>% dplyr::mutate(season = season, week = week, gameday = stringr::str_extract(gametime, "^[A-Za-z]{3}"), gametime = stringr::str_extract(gametime,
        "\\d+:\\d+ [PAM]+")) %>% dplyr::select(season, week, name, position, team, opponent, gameday, gametime, percent_owned, injury, projection))
    return(playerData)
}
