#scrape ODI results
library(data.table)
library(rvest)
library(stringr)
library(tidyr)
library(ggplot2)
library(curl)
library(moments)
library(normtest)
site <- paste('http://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;orderby=result;size=200;spanmax1=11+Jan+2017;spanmin1=1+Jan+2007;spanval1=span;template=results;type=team;view=innings;page='jump)
#fUCK THIS

#do for all 14
url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;orderby=result;page=14;size=200;spanmax1=11+Jan+2017;spanmin1=1+Jan+2007;spanval1=span;template=results;type=team;view=innings"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
p14 <- html_table(sb_table,fill = T)[[3]]

#Bind them all
full <- rbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14)

#remove unnecessary columns
full[,7]<-NULL
full[,10]<-NULL

#normalize the date
full$`Start Date` <-gsub(' ','\\-',full$`Start Date`)
full$`Start Date` <-gsub('20','',full$`Start Date`)
full$`Start Date` <- as.Date(as.character(full$`Start Date`),format="%d-%b-%y")

#Save 
write.csv(full,"ODI_Results.csv")
#Find matches between only top 8 teams
Top8 <- full[grep("Australia|England|India|New Zealand|Pakistan|Sri Lanka|South Africa|West Indies",full$Team),]
Top8 <- Top8[grep("Australia|England|India|New Zealand|Pakistan|Sri Lanka|South Africa|West Indies",Top8$Opposition),]
Top8 -> dat #Easy to type
write.csv(dat,"Top8ODI.csv")

#Remove matches where team does not bat

dat <- dat[dat$Score != 'DNB' ,]

#Simplify score into runs scored and wickets fallen

dat$WicketsFallen <- str_sub(dat$Score,-1)
dat$RunScored <- sapply(strsplit(dat$Score, "/"), `[[`, 1) #YAY
dat$RunScored <- as.numeric(dat$RunScored)

#Remove the no result games

dat <- dat[dat$Result !='n/r',]
skewness(dat$RunScored)#-0.064
kurtosis(dat$RunScored)#3.01
kurtosis(dat$RunScored) -3 #excess kurtosis >0 =>LeptoKurtic(Fat tail)
hist(dat$RunScored)
mean(dat$RunScored) #237.3
median(dat$RunScored)
var(dat$RunScored)#3873.743

#Drawing a normal dist with same mean and variance
x <- seq(0,430,0.01)
hist(dat$RunScored)
par(new = T)
plot(dnorm(x,mean = mean(dat$RunScored),sd = sd(dat$RunScored)))
#Fits well-ish

#Split the data into 1st and 2nd innings
in1 <- dat[dat$Inns==1,]
in1$Inns <- NULL 
in2 <- dat[dat$Inns!=1,]
#adjusted jarque bera test to check for normality
ajb.norm.test(in1$RunScored,nrepl = 10000) #p value of 0.0568
#null hypotheisi cannot be rejected
#We assume the scores are distributed normally

#estimation of probability of winning
table(in1$Result)
in1 <- in1[in1$Result !='tied',]
#Predict using logit model
in1$BinResult <- ifelse(in1$Result=='won',1,0)
model <- glm(BinResult~RunScored,family = binomial(link = 'logit'),data = in1)
summary(model)
#Finding the mean and variance of our 2nd innings distribution
SecondM <- 5.737152/0.022193
SecondVar <- 1/(0.013127)^2
pred <- predict(model, type = 'response')#Pred is our 2nd innings distribution
#Mean = 258.511, var = 5803.219
258.511-mean(in1$RunScored)
#Difference of 3.78 runs between 1st and 2nd innings mean

#Calculate delta and exp. 2nd innings variance

