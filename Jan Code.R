# Pfad
math <- read.table(file="C:/Users/Jan/OneDrive/Dokumente/Studium/4_Semester/Data Exploration/Datenset/student-mat.csv", sep = ",", header=TRUE)
port <- read.table(file="C:/Users/Jan/OneDrive/Dokumente/Studium/4_Semester/Data Exploration/Datenset/student-por.csv", sep = ",", header=TRUE)


#basic statistics
install.packages("ggplot2", dependencies=TRUE)
#load ggplot2
library(ggplot2)

library(dplyr)
library(rpart)
install.packages("rpart.plot")
library("rpart.plot")
library(corrplot)
install.packages('Rcpp')
library(Hmisc)
install.packages("tidyverse")
library(tidyverse)

port$G_average <- (port$G1 + port$G2 + port$G3)/3
port <- port %>% select(-G1,-G2,-G3)


#Vergleich der Noten bei unterschiedlichen Schulen
###########EDA School
table(port_EDA$school)
port_EDA$G_average_mean_school[port$school=="GP"] <- mean(port_EDA$G_average[port$school=="GP"],)
port_EDA$G_average_mean_school[port$school=="MS"] <- mean(port_EDA$G_average[port$school=="MS"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=school, color=school)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.4)+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_school, color=school),linetype="dashed")+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))
p

## medu
# Problem
table(port_EDA$Medu)

port_EDA$G_average_mean_Medu[port$Medu=="0"] <- mean(port_EDA$G_average[port$Medu=="0"],)
port_EDA$G_average_mean_Medu[port$Medu=="1"] <- mean(port_EDA$G_average[port$Medu=="1"],)
port_EDA$G_average_mean_Medu[port$Medu=="2"] <- mean(port_EDA$G_average[port$Medu=="2"],)
port_EDA$G_average_mean_Medu[port$Medu=="3"] <- mean(port_EDA$G_average[port$Medu=="3"],)
port_EDA$G_average_mean_Medu[port$Medu=="4"] <- mean(port_EDA$G_average[port$Medu=="4"],)

port_EDA$Medu <- as.character(port_EDA$Medu)
p<-ggplot(port_EDA, aes(x=G_average, y=Medu)) +
  geom_violin(aes(fill=Medu, color=Medu),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

## age
#Wenig datenpunkte bei age 21 und 22, nur jew 1 und 2 Personen
table(port_EDA$age)


port_EDA$age <- as.character(port_EDA$age)
p<-ggplot(port_EDA[port_EDA$age<=19,], aes(x=G_average, y=age)) +
  geom_violin(aes(fill=age, color=age),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p


# famrel
table(port_EDA$famrel)


port_EDA$famrel <- as.character(port_EDA$famrel)
p<-ggplot(port_EDA[port_EDA$famrel<=5,], aes(x=G_average, y=famrel)) +
  geom_violin(aes(fill=famrel, color=famrel),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c())+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

# freetime
table(port_EDA$freetime)


port_EDA$freetime <- as.character(port_EDA$freetime)
p<-ggplot(port_EDA[port_EDA$freetime<=5,], aes(x=G_average, y=freetime)) +
  geom_violin(aes(fill=freetime, color=freetime),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c())+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p
