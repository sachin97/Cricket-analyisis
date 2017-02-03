library(rvest)
library(stringr)
library(tidyr)
library(ggplot2)

url <- "http://stats.espncricinfo.com/syed-mushtaq-ali-trophy-2015-16/engine/records/batting/most_runs_career.html?id=10287;type=tournament"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
SMAT_Bat <- html_table(sb_table,fill = T)[[4]]
#Remove * in highest score
SMAT_Bat$HS <- ifelse ((str_sub(SMAT_Bat$HS,-1)=='*'), gsub('\\*','',SMAT_Bat$HS), SMAT_Bat$HS)

#Convert to numeric
x <- as.data.frame(sapply(SMAT_Bat, as.numeric)
x$Player <- SMAT_Bat$Player
SMAT_Bat <- x

SMAT_Bat -> full
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


#Plot
ggplot(full,aes(x=Runs,y = BoundaryPercent))+geom_point()+geom_text(aes(label = ifelse(RPI>34.625,as.character(Player),''),hjust = 0, vjust = 0,color='red'))
#Finishers
ggplot(full,aes(x=BallsFacedPI,y = SR))+geom_point()+geom_text(aes(label = ifelse((SR>142.8625&BallsFacedPI<= 20.19643),as.character(Player),''),hjust = 0, vjust = 0,color='red'))
#Top order run machines
g <-ggplot(full,aes(x=RPI,y =SR ))+geom_point()+geom_text(aes(label = ifelse((RPI>34.625&SR>142.8625),as.character(Player),''),hjust = 0, vjust = 0,color='red'))
g <- g+geom_hline(aes(yintercept = median(full$SR)),size = 1.5,alpha = 0.7)
g <- g+geom_vline(aes(xintercept = median(full$RPI)),size = 1.5,alpha = 0.7)

ggplot (full,aes(x = RPI,y = BoundaryPercent))+geom_point()+geom_text(aes(label = as.character(Player),color = 'black'))

full$NewAve <- (full$Runs)/ (full$Inns)
