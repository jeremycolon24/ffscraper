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
        timeLength <- transDetails %>% stringr::str_extract('\\d+ [a-z]+ ago$') %>% stringr::str_extract('^\\d+') %>% as.integer()
        timePeriod <- transDetails %>% stringr::str_extract('\\d+ [a-z]+ ago$') %>% stringr::str_extract(' [a-z]+ ') %>% stringr::str_trim()
        multiplier <- case_when(
          stringr::str_detect(timePeriod,'minute') ~ 60,
          stringr::str_detect(timePeriod,'hour') ~ 60*60,
          stringr::str_detect(timePeriod,'day') ~ 60*60*24,
          stringr::str_detect(timePeriod,'week') ~ 60*60*24*7,
          stringr::str_detect(timePeriod,'month') ~ 60*60*24*30,
          stringr::str_detect(timePeriod,'year') ~ 60*60*24*365,
          TRUE ~ 1
        )
        transactionDate <- as.Date(Sys.time() - (timeLength*multiplier))
        if(t == "CLAIM"){
          playerName <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ Claimed [A-Za-z.' -]+ \\(") %>% stringr::str_remove("^[A-Za-z0-9(),' ]+ Claimed ") %>% stringr::str_remove(' \\($')
          transactionDetail <- transDetails %>% stringr::str_extract('\\$\\d+')
        } else if(t == "ADD") {
          playerName <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ Added [A-Za-z.' -]+ \\d+") %>% stringr::str_remove("^[A-Za-z0-9(),' ]+ Added ") %>% stringr::str_remove(' \\d+$')
          transactionDetail <- ""
        } else {
          playerName <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ Cut [A-Za-z.' -]+ \\d+") %>% stringr::str_remove("^[A-Za-z0-9(),' ]+ Cut ") %>% stringr::str_remove(' \\d+$')
          transactionDetail <- ""
        }
        transData <- rbind(transData, data.frame(transactionDate, transType = t, teamID = id, playerName, transactionDetail))
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
#' @param transLogLink Web link to transaction log
#' @param leagueLink Web link to main league page
#' @param team Accepts Team ID to get transactions for. Defaults to ALL
#' @return Trades
#' @examples
#' getTrades(tradesLink)
#' @export
getTrades <- function(transLogLink, leagueLink, team = 'ALL'){

  teams <- ffscraper::getTeams(leagueLink)
  if(team != "ALL" & !(team %in% teams['teamID'])){
    stop("Not a valid team ID. Use getTeams function to see your League's team IDs")
  }

  tradeData <- data.frame(tradeID = integer(), transactionDate = character(), fromTeam = integer(), assetName = character(), toTeam = integer(), tradeLink = character())

  for(i in 1:length(team)) {
    tableOffset <- 0
    if(team == "ALL"){
      tradesLink <- paste0(transLogLink,"?transactionType=TRADE&tableOffset=",tableOffset)
    } else {
      tradesLink <- paste0(transLogLink,"?transactionType=TRADE&teamId=",team[i],"&tableOffset=",tableOffset)
    }
    transactions <- xml2::read_html(tradesLink)
    transDetails <- transactions %>% rvest::html_nodes(".list-group-item-text") %>% rvest::html_text()

    while(length(transDetails) > 0){
      timeLength <- transDetails %>% stringr::str_extract('\\d+ [a-z]+ ago$') %>% stringr::str_extract('^\\d+') %>% as.integer()
      timePeriod <- transDetails %>% stringr::str_extract('\\d+ [a-z]+ ago$') %>% stringr::str_extract(' [a-z]+ ') %>% stringr::str_trim()
      multiplier <- case_when(
        stringr::str_detect(timePeriod,'minute') ~ 60,
        stringr::str_detect(timePeriod,'hour') ~ 60*60,
        stringr::str_detect(timePeriod,'day') ~ 60*60*24,
        stringr::str_detect(timePeriod,'week') ~ 60*60*24*7,
        stringr::str_detect(timePeriod,'month') ~ 60*60*24*30,
        stringr::str_detect(timePeriod,'year') ~ 60*60*24*365,
        TRUE ~ 1
      )
      transactionDate <- as.Date(Sys.time() - (timeLength*multiplier))
      assetName <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ Traded For .* from [A-Za-z0-9(),' ]+ \\d+") %>% stringr::str_remove("^[A-Za-z0-9(),' ]+ Traded For ") %>% stringr::str_remove(" from [A-Za-z0-9(),' ]+ \\d+") %>% stringr::str_remove(' \\d+$')
      toTeam <- transDetails %>% stringr::str_extract("^[A-Za-z0-9(),' ]+ Traded For") %>% stringr::str_remove(" Traded For")
      fromTeam <- transDetails %>% stringr::str_extract(" from [A-Za-z0-9(),' ]+ \\d+") %>% stringr::str_remove(" from ") %>% stringr::str_remove(' \\d+$')
      tradeLink <- transactions %>% rvest::html_nodes(".list-group-item-text a")
      tradeLink <- paste0("https://www.fleaflicker.com",tradeLink[which(grepl("trades",tradeLink))] %>% rvest::html_attr("href"))
      tradeID <- tradeLink %>% stringr::str_extract("\\d+$")
      tradeData <- rbind(tradeData, data.frame(tradeID, transactionDate, fromTeam, assetName, toTeam, tradeLink))

      tableOffset <- tableOffset + 15
      if(team == "ALL"){
        tradesLink <- paste0(transLogLink,"?transactionType=TRADE&tableOffset=",tableOffset)
      } else {
        tradesLink <- paste0(transLogLink,"?transactionType=TRADE&teamId=",team[i],"&tableOffset=",tableOffset)
      }
      transactions <- xml2::read_html(tradesLink)
      transDetails <- transactions %>% rvest::html_nodes(".list-group-item-text") %>% rvest::html_text()

    }
  }

  return(distinct(tradeData))
}



