devtools::install_github('jeremycolon24/ffscraper', ref = 'develop')
library(googlesheets)
library(ffscraper)
library(magrittr)
library(dplyr)

season <- 2019
ffscraper::getLinks(197269,season)
teams <- ffscraper::getTeams(leagueLink)

getDraftPicks <- function(teams) {
  picks_df <- data.frame(draftSeason = numeric(), teamID = numeric(), roundPick = character(), overallPick = numeric(), origin_destination = character(), to_from_team = character(), trade_link = character(), pick_used = character())
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
      pick_used <- pick_tr[j] %>% rvest::html_nodes("em")
      if(length(pick_used)){
        pick_used <- pick_used %>% rvest::html_text()
      } else {
        pick_used <- "Used"
      }
      pickData <- pick_tr[j] %>% rvest::html_nodes("td .text-muted")
      if(length(pickData)){
        pickData <- pick_tr[j]
        roundPick <- (pickData %>% rvest::html_nodes("td") %>% rvest::html_text())[2] %>% stringr::str_remove(' #\\d+$')
        overallPick <- pickData %>% rvest::html_nodes("td .text-muted") %>% rvest::html_text() %>% stringr::str_remove('#') %>% as.numeric()
        status <-  pickData %>% rvest::html_nodes("td a") %>% stringr::str_extract('Obtained|Traded') %>% na.omit() %>% as.character()
        status <- if(length(status)) {status} else {"Own"}
        status <- if(status == 'Obtained') {"Acquired"} else {status}
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
        picks_df <- rbind(picks_df, data.frame(draftSeason = draftSeason, teamID = teamID, roundPick = roundPick, overallPick = overallPick, status = status, to_from_team = to_from_team, trade_link = trade_link, pick_used = pick_used))
      }
    }
  }
  return(picks_df)
}

teamID <- teams[1, "teamID"]
picksLink <- paste0(as.character(teams[1, "teamLink"]),"/picks")
picks <- xml2::read_html(picksLink)
picks %>% rvest::html_nodes("table") %>% rvest::html_nodes("tr")
pick_tr <- picks %>% rvest::html_nodes("table") %>% rvest::html_nodes("tr")
pick_tr %>% rvest::html_nodes("h3") %>% rvest::html_text() %>% stringr::str_remove(" Draft Picks") %>% as.numeric()
pick_tr[4] %>% rvest::html_nodes("td a") %>% rvest::html_attr("href") %>% stringr::str_extract(".*trades.*") %>% na.omit() %>% as.character()
pick_tr <- pick_tr[c(3:length(pick_tr))]
pick_tr %>% rvest::html_nodes("td .text-muted") %>% rvest::html_text()
pick_tr %>% rvest::html_nodes("em")
pickData <- pick_tr[1] %>% rvest::html_nodes("code")


picks_df %>%
  filter(status != 'Traded', pick_used == "Used") %>%
  arrange(draftSeason,overallPick) %>%
  View()



