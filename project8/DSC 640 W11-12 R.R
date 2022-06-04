library(readr)
library(ggplot2)
library(dplyr)
getwd()
setwd("~/Documents/DSC 640")
df<- read_csv("w11-12/education.csv")
df
math<-df$math
read<-df$reading
write<-df$writing
p <- ggplot(df, aes(x=math)) + geom_histogram(binwidth=10,fill="darkblue")
p
q<-ggplot(df, aes(x=math)) + geom_boxplot(color="darkblue")+ coord_flip()
q
library(tidyverse)
m<-ggplot(df, aes(x = math, y = df$state),color=Species) + geom_col(fill="blue") 
m
library(reshape2)
df1 <- df[, c('math','reading','writing')]
df1
plot(1:length(math), math)
points(1:length(read), read, col=2)
points(1:length(write), write, col=3)
