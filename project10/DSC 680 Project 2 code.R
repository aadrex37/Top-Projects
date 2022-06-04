library(ggplot2)
getwd()
setwd("~/Documents/DSC 680")
##SET DIRECTORY
library(readr)
df <-read_csv("tmdb-movies.csv")
View(df)
##IMPORT DATA
df1<- df[, c('popularity','budget','revenue')]
View(df1)
##REDUCED VARIABLES TO THOSE WE WANT
profit <- df1$revenue-df1$budget
profit
##CREATED A NEW VARIABLE, JOINT EDUCATION LEVEL
df1$profit <- profit
View(df1)
##INSERT BACK INTO DF
corr <- round(cor(df1),2)
View(corr)
##FINDS CORRELATIONS OF VARIABLES
library(reshape2)
melted_corr <- melt(corr)
View(melted_corr)
##MELTS DATA TO BE ABLE TO PUT INTO HEAT MAP
ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
##GRAPHED MELTED DF INTO HEAT MAP
install.packages("dplyr") 
install.packages("caTools")
install.packages("ROCR") 
##INSTALLING PACKAGES REQUIRED FOR LOGISTIC REGRESSION
library(dplyr)
library(caTools) 
##loading packages
profit_lm <- lm(profit ~ popularity, data = df1)
summary(profit_lm)
##LINEAR REGRESSION MODEL
plot(profit_lm)
##Check for homoscedasticity
profit_graph<-ggplot(df1, aes(x=profit, y=popularity))+geom_point()+
  geom_smooth(method="lm", col="red")+theme_bw() +
  labs(title = "Movie profits",
       x = "Budget",
       y = "Revenue")
profit_graph
##PLOT THE DATA WITH LINEAR REGRESSION MODEL
