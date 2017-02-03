#FUCK R
#scrape data
library(data.table)
library(rvest)
library(stringr)
library(tidyr)
library(ggplot2)
library(curl)
library(moments)
library(normtest)


url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?batting_positionmax1=3;batting_positionmin1=1;batting_positionval1=batting_position;class=3;filter=advanced;opposition=1;opposition=2;opposition=25;opposition=3;opposition=4;opposition=5;opposition=6;opposition=7;opposition=8;orderby=runs;qualmin1=10;qualval1=matches;size=200;spanmax1=25+Jan+2017;spanmin1=25+Jan+2013;spanval1=span;team=1;team=2;team=25;team=3;team=4;team=5;team=6;team=7;team=8;template=results;type=batting"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
full <- html_table(sb_table,fill = T)[[3]]

full[,16] <- NULL
colnames(full)[15] <- "X6s"
colnames(full)[14] <- "X4s"
colnames(full)[13] <- "X0s"
colnames(full)[12] <- "X50s"
colnames(full)[11] <- "X100s"

full$SixPI <- (full$X6s)/(full$Inns) #sixes per innings
full$FourPI <- (full$X4s)/(full$Inns) #fours per innings
full$BoundaryPI <- full$FourPI + full$SixPI
full$RPI <- (full$Runs)/full$Inns
full$SixPercent <- ((full$SixPI)/(full$RPI))*600
full$FourPercent <- ((full$FourPI)/(full$RPI))*400
full$BoundaryPercent <- full$SixPercent + full$FourPercent
full$BallsFacedPI <- (full$BF)/(full$Inns)
full$NOPI <- full$NO/full$Inns

write.csv(full,"T20IntlBat.csv")

#Draw a radar chart
library(fmsb)
#The limits are mean+/-2 standard deviations
df <- data.frame(full[full$Player=='V Kohli' ,colnames <-c("BoundaryPI",'RPI','BallsFacedPI','SR','NOPI','BoundaryPercent')])#Probably execute this first!
df[1,] <- c((mean(full$BoundaryPI) + 2*sd(full$BoundaryPI)),(mean(full$RPI) + 2*sd(full$RPI)),(mean(full$BallsFacedPI) + 2*sd(full$BallsFacedPI)),(mean(full$SR) + 2*sd(full$SR)),(mean(full$NOPI) + 2*sd(full$NOPI)),(mean(full$BoundaryPercent) + 2*sd(full$BoundaryPercent)))
df[2,] <- c((mean(full$BoundaryPI) - 2*sd(full$BoundaryPI)),(mean(full$RPI) - 2*sd(full$RPI)),(mean(full$BallsFacedPI) - 2*sd(full$BallsFacedPI)),(mean(full$SR) - 2*sd(full$SR)),(mean(full$NOPI) - 2*sd(full$NOPI)),(mean(full$BoundaryPercent) - 2*sd(full$BoundaryPercent)))
df[3,] <- full[full$Player=='CH Gayle' ,colnames <-c("BoundaryPI",'RPI','BallsFacedPI','SR','NOPI','BoundaryPercent')]#Probably execute this first!
df[4,] <- full[full$Player=='DR Smith' ,colnames <-c("BoundaryPI",'RPI','BallsFacedPI','SR','NOPI','BoundaryPercent')]#Probably execute this first!

library(xlsx)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( df , axistype=2 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 ,seg = 4,title = 'BB McCullum')
)

#mine more data
#Already have IPL data
dat <- read.csv("All_Batting.csv")
full <- dat
#iNTRODUCE AN INNINGS CUTOFF
full <- full[full$Inns > 20,]
#Fuse all rows with the same name
write.csv(full,"IPL_Bats.csv")
full$Span <- NULL
library(plyr)
#full$SR <- as.factor(full$SR)
full <- ddply(full,"Player",numcolwise(mean))
write.csv(full1,"IPLBatTeamFuse.csv")
full <- full1
#too much variance
#Add run Cutoff
quantile(full$Runs,0.1)
full <- full[full$Runs>345 ,]
