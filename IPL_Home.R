#Home advantage analysis
library(rvest)
library(stringr)
library(tidyr)
library(ggplot2)
library(data.table)

url <- "http://stats.espncricinfo.com/indian-premier-league-2016/engine/records/team/match_results.html?id=11001;type=tournament"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
res <- html_table(sb_table,fill = T)[[13]]

res$Scorecard <- NULL#Remove unnecessary column
res$`Match Date` <- NULL
#Team 1 is the home team and team2 is the away team
#Remove Ground as it is unnecessary
res$Ground <- NULL
#Count Home Wins
res1 <- res1[1:56,] #removing playoff places
HWin <- res1[res1$`Team 1`==res1$Winner,]
nrow(HWin)
#Holy shit!
#Maybe we need more data


#2015
url <- "http://stats.espncricinfo.com/indian-premier-league-2015/engine/records/team/match_results.html?id=9657;type=tournament"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
res2015 <- html_table(sb_table,fill = T)[[13]]
#2014
url <- "http://stats.espncricinfo.com/indian-premier-league-2014/engine/records/team/match_results.html?id=8827;type=tournament"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
res2014 <- html_table(sb_table,fill = T)[[16]]
#2013
url <- "http://stats.espncricinfo.com/indian-premier-league-2013/engine/records/team/match_results.html?id=7720;type=tournament"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
res2013 <- html_table(sb_table,fill = T)[[13]]
#2012
url <- "http://stats.espncricinfo.com/indian-premier-league-2012/engine/records/team/match_results.html?id=6680;type=tournament"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
res2012 <- html_table(sb_table,fill = T)[[13]]
#2011
url <- "http://stats.espncricinfo.com/indian-premier-league-2011/engine/records/team/match_results.html?id=5969;type=tournament"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
res2011 <- html_table(sb_table,fill = T)[[10]]

#Probably showldhave used a function, oh well

#Binding all the individual datasets

full <- rbind(res,res2015,res2014,res2013)
#406, nice
write.csv(full,"IPL_Results.csv")
#Calc number of home wins
HWin <- (full[full$`Team 1`==full$Winner,])
HWin
(139/256)*100

#54% of wins are home wins.

#Check by team
summary(full$`Team 1`)
full$`Team 1` <- as.factor(full$`Team 1`)
full$Ground1 <- as.factor(full$Ground)
#Fraction of wins for each side in a ground is to be calculated
WinFraction <- function(g,w)
{
  HWFrac <- nrow(full[full$Winner==w&full$Ground==g,])/nrow(full[full$Ground == g,])#Fraction of games a team has won at a particular ground
  return(HWFrac)
}
#***Calc for each team and create viz later***
barplot(c(WinFraction('Chennai','Super Kings'),WinFraction('Bangalore','RCB'),WinFraction('Delhi','Daredevils')))

#home wins are not very significant
