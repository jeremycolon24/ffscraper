# Transactions
#
# A set of functions that pull transactions.
#

#' Gets all transactions from transaction log
#'
#' @param transLogLink Web link to transaction log
#' @param leagueLink Web link to main league page
#' @param transType Type of transaction. Defaults to ALL.
#'     Current accepted values are c("ADD","CLAIM","DROP").
#' @param team Accepts Team ID to get transactions for. Defaults to ALL
#' @return Transactions
#' @examples
#' getTransactions(transLogLink, "ALL", "ALL", "ALL")
#' getTransactions(transLogLink, "CLAIM", 100, 1234567)
#' @export
getTransactions <- function(transLogLink, leagueLink, transType = 'ALL', team = 'ALL') {
  transTypes <- c("ADD","CLAIM","DROP")
  if(transType == 'ALL') {
    transType <- transTypes
  } else {
    if(!(transType %in% transTypes)){
      stop("Not a valid transaction type. See documentation for accepted types")
    }
  }

  teams <- ffscraper::getTeams(leagueLink)
  if(team != "ALL" & !(team %in% teams['teamID'])){
    stop("Not a valid team ID. Use getTeams function to see your League's team IDs")
  }

  transData <- data.frame(transactionDate = character(), transactionType = character(), teamID = numeric(), playerName = character(), transactionDetail = character())
  for(id in teams$teamID) {
    for(t in transType) {
      tableOffset <- 0
      transLogLinkParams <- paste0(transLogLink,"?transactionType=",t,"&teamId=",id,"&tableOffset=",tableOffset)
      transactions <- xml2::read_html(transLogLinkParams)
      transDetails <- transactions %>% rvest::html_nodes(".list-group-item-text") %>% rvest::html_text()
      while(length(transDetails) > 0){

        script <- transactions %>%
          rvest::html_nodes("script#page-data") %>%
          rvest::html_text() %>%
          stringr::str_remove('^window.pageData = \\{"tooltips.:.') %>%
          stringr::str_remove('..;$')

        script <- stringr::str_split(script,'\\},\\{')[[1]]
        script <- as.data.frame(script)
        colnames(script) <- "content"
        script$content <- as.character(script$content)

        transactionDates <- script %>%
          mutate(
            transactionDate = stringr::str_extract(content,'"contents":.[A-Za-z]{3} \\d+/\\d+/\\d+ \\d+:\\d+[ ]?[AP]M'),
            ids = stringr::str_extract(content,paste0(transactionDate,'.+ids":\\["[A-Za-z0-9_,"]+\\]')),
            ids = stringr::str_remove(ids, transactionDate),
            ids = stringr::str_extract(ids, '\\["[A-Za-z0-9_,"]+\\]'),
            ids = stringr::str_count(ids, ",") + 1,
            transactionDate = as.POSIXct(stringr::str_remove(transactionDate,'"contents":"'), format="%a %m/%d/%y %I:%M %p")
          ) %>%
          select(transactionDate, ids) %>%
          filter(!is.na(transactionDate)) %>%
          arrange(desc(transactionDate))

        transactionDates <- transactionDates[rep(seq_len(nrow(transactionDates)), transactionDates$ids),"transactionDate"]

        if(t == "CLAIM"){
          playerName <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ [Cc]laimed [A-Za-z.' -]+ \\(") %>% stringr::str_remove("^[A-Za-z0-9(),' ]+ [Cc]laimed ") %>% stringr::str_remove(' \\($')
          transactionDetail <- transDetails %>% stringr::str_extract('\\$\\d+')
        } else if(t == "ADD") {
          playerName <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ [Aa]dded [A-Za-z.' -]+ \\d+") %>% stringr::str_remove("^[A-Za-z0-9(),' ]+ [Aa]dded ") %>% stringr::str_remove(' \\d+$')
          transactionDetail <- ""
        } else {
          playerName <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ [Cc]ut [A-Za-z.' -]+ \\d+") %>% stringr::str_remove("^[A-Za-z0-9(),' ]+ [Cc]ut ") %>% stringr::str_remove(' \\d+$')
          transactionDetail <- ""
        }
        transData <- rbind(transData, data.frame(transactionDates, transType = t, teamID = id, playerName, transactionDetail))
        tableOffset <- tableOffset + 15
        transLogLinkParams <- paste0(transLogLink,"?transactionType=",t,"&teamId=",id,"&tableOffset=",tableOffset)
        transactions <- xml2::read_html(transLogLinkParams)
        transDetails <- transactions %>% rvest::html_nodes(".list-group-item-text") %>% rvest::html_text()
      }
    }
  }

  return(transData)

}

#' Gets all trades from trades page
#'
#' @param tradesLink Web link to trade log
#' @return Trades
#' @examples
#' getTrades(tradesLink)
#' @export
getTrades <- function(tradesLink){

  tradeData <- data.frame(tradeID = integer(), transactionDate = character(), heading = character(), fromTeam = integer(), asset = character(), toTeam = integer(), vetoes = integer(), tradeLink = character())
  tableOffset <- 0
  link <- paste0(tradesLink,"?status=EXECUTED&tableOffset=",tableOffset)
  trades <- xml2::read_html(link)
  tradeDetails <- trades %>% rvest::html_nodes(".list-group-item-text")

  while(length(tradeDetails) > 0){
    heading <- trades %>% rvest::html_nodes(".list-group-item-heading") %>% rvest::html_text() %>% stringr::str_remove(" Complete")
    transactionDates <- tradeDetails %>% rvest::html_nodes(".relative-date") %>% rvest::html_attr("title") %>% as.POSIXct(format="%a %m/%d/%y %I:%M %p")
    vetoes <- tradeDetails %>% rvest::html_nodes(".media-info") %>% rvest::html_text() %>% stringr::str_extract('\\d Vetoes') %>% stringr::str_extract('\\d') %>% as.integer()
    tradeLink <- tradeDetails %>% rvest::html_nodes(".btn-xs")
    tradeLink <- paste0("https://www.fleaflicker.com",tradeLink[which(grepl("trades",tradeLink))] %>% rvest::html_attr("href"))
    tradeID <- tradeLink %>% stringr::str_extract("\\d+$")
    for(i in seq(1,length(tradeDetails))){
      trade <- tradeDetails[i]
      tradeTeams <- trade %>% rvest::html_nodes("div .league-name") %>% rvest::html_text() %>% unique()
      for(j in seq(1,length(tradeTeams))){
        toTeam <- tradeTeams[j]
        if(length(tradeTeams) == 2){
          fromTeam <- tradeTeams[-j]
          assets <- (trade %>% rvest::html_nodes("ul"))[j] %>% rvest::html_nodes("li")
          if(length(assets) > 0){
            for(k in seq(1,length(assets))){
              if(length(assets[k] %>% rvest::html_nodes(".player-text") %>% rvest::html_text()) > 0){
                asset <- assets[k] %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
              } else {
                asset <- assets[k] %>% rvest::html_text()
              }
              tradeData <- rbind(tradeData, data.frame(tradeID[i], transactionDates[i], heading[i], fromTeam, asset, toTeam, vetoes[i], tradeLink[i]))
            }
          }
        } else {
          assets <- (trade %>% rvest::html_nodes("ul"))[j] %>% rvest::html_nodes("li")
          if(length(assets) > 0){
            for(k in seq(1,length(assets))){
              fromTeam <- assets[k] %>% rvest::html_text() %>% stringr::str_extract("from .*") %>% stringr::str_remove("from ")
              if(length(assets[k] %>% rvest::html_nodes(".player-text") %>% rvest::html_text()) > 0){
                asset <- assets[k] %>% rvest::html_nodes(".player-text") %>% rvest::html_text()
              } else {
                asset <- assets[k] %>% rvest::html_text()
              }
              tradeData <- rbind(tradeData, data.frame(tradeID[i], transactionDates[i], heading[i], fromTeam, asset, toTeam, vetoes[i], tradeLink[i]))
            }
          }
        }
      }
    }
    tableOffset <- tableOffset + 5
    link <- paste0(tradesLink,"?status=EXECUTED&tableOffset=",tableOffset)
    trades <- xml2::read_html(link)
    tradeDetails <- trades %>% rvest::html_nodes(".list-group-item-text")

  }
  colnames(tradeData) <- c('tradeID','transactionDate','tradeHeading','fromTeam','asset','toTeam','vetoes','tradeLink')
  return(distinct(tradeData))
}



