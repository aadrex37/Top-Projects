library(ggplot2)
getwd()
setwd("~/Documents/DSC 680")
##SET DIRECTORY
library(readr)
df <-read_csv("student-por.csv")
View(df)
##IMPORT DATA
library(dplyr)
df1 <- df[, c('G1','G2','G3','studytime','activities','romantic','freetime',
              'goout','absences','Medu','Fedu','Pstatus','traveltime',
              'schoolsup','famsup','paid','nursery','higher','internet','famrel'
              ,'health')]
View(df1)
##REDUCED VARIABLES TO THOSE WE WANT
famedu <- df1$Medu + df1$Fedu
famedu
##CREATED A NEW VARIABLE, JOINT EDUCATION LEVEL
df1$Famedu <- famedu
View(df1)
##INSERT BACK INTO DF
df1 <- df1[, c('G1','G2','G3','studytime','activities','romantic','freetime',
              'goout','absences','Medu','Fedu','Famedu','Pstatus','traveltime',
              'schoolsup','famsup','paid','nursery','higher','internet','famrel'
              ,'health')]
View(df1)
##REORDERED TO PUT FAM EDU NEXT TO INDIVIDUAL PARENT ED
df1$activities<-ifelse(df1$activities=="yes",1,0)
df1$romantic<-ifelse(df1$romantic=="yes",1,0)
df1$Pstatus<-ifelse(df1$Pstatus=="T",1,0)
df1$schoolsup<-ifelse(df1$schoolsup=="yes",1,0)
df1$famsup<-ifelse(df1$famsup=="yes",1,0)
df1$paid<-ifelse(df1$paid=="yes",1,0)
df1$nursery<-ifelse(df1$nursery=="yes",1,0)
df1$higher<-ifelse(df1$higher=="yes",1,0)
df1$internet<-ifelse(df1$internet=="yes",1,0)
View(df1)
##CONVERTED CATEGORICAL DATA INTO NUMERICAL
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
