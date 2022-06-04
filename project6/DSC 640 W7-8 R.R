library(ggplot2)
getwd()
setwd("~/Documents/DSC 640")
library(readr)
df <- read_csv("birth-rates-yearly.csv")
df1 <-read_csv("crimerates-by-state-2005.csv")
View(df)
View(df1)
library("dplyr")
avg <- df %>% group_by(year)%>% summarise(rate = sum(rate),.groups = 'drop')
View(avg)
year<-avg$year
rate<-avg$rate
ggplot(avg, aes(x=year, y=rate)) + geom_point()
df2<-df1[-c(1),] 
View(df2)
df3<-df2[-c(9),]
View(df3)
pop<-df3$population
mrdr<-df3$murder
rob<- df3$robbery
ggplot(df3, aes(x=pop, y=mrdr, size = rob)) + geom_point(alpha=0.5)
d <- density(year)
plot(d)
