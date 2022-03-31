#Ggf. installieren der benötigten Libraries
install.packages(boot)
install.packages(glmnet) 
install.packages(dplyr)
install.packages(rpart)
install.packages(rpart.plot)
install.packages(corrplot)
install.packages(Hmisc)
install.packages(tidyverse)
install.packages(ggplot2)
install.packages(fastDummies)

#Laden der benötigten Libraries
library(boot)
library(glmnet) 
library(dplyr)
library(rpart)
library(rpart.plot)
library(corrplot)
library(Hmisc)
library(tidyverse)
library(ggplot2)
library(fastDummies)


#Leon
port <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-por.csv", sep = ",", header=TRUE)
#Yannick
port <- read.table(file="C:/Temp/student-por.csv", sep = ",", header=TRUE)
#Jan
port <- read.table(file="C:/Users/Jan/OneDrive/Dokumente/Studium/4_Semester/Data Exploration/Datenset/student-por.csv", sep = ",", header=TRUE)


#Aufteilen in Trainings und Testdaten, 3 verscheiden Aufteilungen

#Aufteilung 1
set.seed(12)

trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]

#Aufteilung 2
set.seed(42)

trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]

#Aufteilung 3
set.seed(100)

trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]




#Durchschnittsnote
port$G_average <- (port$G1 + port$G2 + port$G3)/3 #Die 3 Teilnoten werden in eine Durchschnittsnote überführt
port <- port %>% select(-G1,-G2,-G3) # Die 3 Teilnoten werden aus dem Datensatz entfernt



#Explorative Datenanalyse

sum(is.na(port)) #keine NAs, sehr gnädiger Datensatz hinsichtlich der Datenaufbereinigung
summary(port$G_average)
hist(port$G_average)
sd(port$G_average)
var(port$G_average)



port_EDA <- port #Anlegen eines extra Datensatzes, an dem Anpassungen für die EDA durchgeführt werden können
View(port_EDA)

#Korrelationsmatrix
cordata <- port %>% dplyr::select(where(is.numeric)) #Filtern der nummerischen Variablen
cordatamatrix <- cor(cordata)#Erstellen einer Korrelationsmatrix
corrplot(cordatamatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#scool
table(port_EDA$school)
port_EDA$G_average_mean_school[port$school=="GP"] <- mean(port_EDA$G_average[port$school=="GP"],)
port_EDA$G_average_mean_school[port$school=="MS"] <- mean(port_EDA$G_average[port$school=="MS"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=school, color=school)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.4)+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_school, color=school),linetype="dashed")+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))
p

#sex
table(port_EDA$sex)

port_EDA$G_average_mean_sex[port$sex=="M"] <- mean(port_EDA$G_average[port$sex=="M"],)
port_EDA$G_average_mean_sex[port$sex=="F"] <- mean(port_EDA$G_average[port$sex=="F"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=sex, color=sex)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_sex, color=sex),linetype="dashed")
p

#age
table(port_EDA$age)
plot(port_EDA$age,port_EDA$G_average)

port_EDA$age <- as.character(port_EDA$age)
p<-ggplot(port_EDA[port_EDA$age<=19,], aes(x=G_average, y=age)) +
  geom_violin(aes(fill=age, color=age),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#adress
table(port_EDA$address)
port_EDA$G_average_mean_address[port$address=="U"] <- mean(port_EDA$G_average[port$address=="U"],)
port_EDA$G_average_mean_address[port$address=="R"] <- mean(port_EDA$G_average[port$address=="R"],)


p<-ggplot(port_EDA, aes(x=G_average, fill=address, color=address)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.4)+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_address, color=address),linetype="dashed")+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))
p

#famsize
table(port_EDA$famsize)
port_EDA$G_average_mean_famsize[port$famsize=="GT3"] <- mean(port_EDA$G_average[port$famsize=="GT3"],)
port_EDA$G_average_mean_famsize[port$famsize=="LE3"] <- mean(port_EDA$G_average[port$famsize=="LE3"],)


p<-ggplot(port_EDA, aes(x=G_average, fill=famsize, color=famsize)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_famsize, color=famsize),linetype="dashed")
p

#Pstatus
table(port_EDA$Pstatus)

port_EDA$G_average_mean_Pstatus[port$Pstatus=="A"] <- mean(port_EDA$G_average[port$Pstatus=="A"],)
port_EDA$G_average_mean_Pstatus[port$Pstatus=="T"] <- mean(port_EDA$G_average[port$Pstatus=="T"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=Pstatus, color=Pstatus)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_Pstatus, color=Pstatus),linetype="dashed")
p

#Medu
table(port_EDA$Medu)


port_EDA$Medu <- as.character(port_EDA$Medu)
p<-ggplot(port_EDA, aes(x=G_average, y=Medu)) +
  geom_violin(aes(fill=Medu, color=Medu),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#fedu
table(port_EDA$Fedu)


port_EDA$Fedu <- as.character(port_EDA$Fedu)
p<-ggplot(port_EDA, aes(x=G_average, y=Fedu)) +
  geom_violin(aes(fill=Fedu, color=Fedu),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#Mjob

table(port_EDA$Fjob)
p<-ggplot(port_EDA, aes(x=G_average, y=Mjob)) +
  geom_violin(aes(fill=Mjob, color=Mjob),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#Fjob

table(port_EDA$Fjob)

p<-ggplot(port_EDA, aes(x=G_average, y=Fjob)) +
  geom_violin(aes(fill=Fjob, color=Fjob),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p


#reason

table(port_EDA$reason)

p<-ggplot(port_EDA, aes(x=G_average, y=reason)) +
  geom_violin(aes(fill=reason, color=reason),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#guardian

table(port_EDA$guardian)


p<-ggplot(port_EDA, aes(x=G_average, y=guardian)) +
  geom_violin(aes(fill=guardian, color=guardian),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#schoolsup
table(port_EDA$schoolsup)

port_EDA$G_average_mean_schoolsup[port$schoolsup=="no"] <- mean(port_EDA$G_average[port$schoolsup=="no"],)
port_EDA$G_average_mean_schoolsup[port$schoolsup=="yes"] <- mean(port_EDA$G_average[port$schoolsup=="yes"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=schoolsup, color=schoolsup)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_schoolsup, color=schoolsup),linetype="dashed")
p

#famsup
table(port_EDA$famsup)

port_EDA$G_average_mean_famsup[port$famsup=="no"] <- mean(port_EDA$G_average[port$famsup=="no"],)
port_EDA$G_average_mean_famsup[port$famsup=="yes"] <- mean(port_EDA$G_average[port$famsup=="yes"],)
p<-ggplot(port_EDA, aes(x=G_average, fill=famsup, color=famsup)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_famsup, color=famsup),linetype="dashed")
p

#paid
table(port_EDA$paid)

port_EDA$G_average_mean_paid[port$paid=="no"] <- mean(port_EDA$G_average[port$paid=="no"],)
port_EDA$G_average_mean_paid[port$paid=="yes"] <- mean(port_EDA$G_average[port$paid=="yes"],)


p<-ggplot(port_EDA, aes(x=G_average, fill=paid, color=paid)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_paid, color=paid),linetype="dashed")
p

#activities
table(port_EDA$activities)

port_EDA$G_average_mean_activities[port$activities=="no"] <- mean(port_EDA$G_average[port$activities=="no"],)
port_EDA$G_average_mean_activities[port$activities=="yes"] <- mean(port_EDA$G_average[port$activities=="yes"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=activities, color=activities)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_activities, color=activities),linetype="dashed")
p

#nursey
table(port_EDA$nursery)

port_EDA$G_average_mean_nursery[port$nursery=="no"] <- mean(port_EDA$G_average[port$nursery=="no"],)
port_EDA$G_average_mean_nursery[port$nursery=="yes"] <- mean(port_EDA$G_average[port$nursery=="yes"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=nursery, color=nursery)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_nursery, color=nursery),linetype="dashed")
p


#higher
table(port_EDA$higher)

port_EDA$G_average_mean_higher[port$higher=="no"] <- mean(port_EDA$G_average[port$higher=="no"],)
port_EDA$G_average_mean_higher[port$higher=="yes"] <- mean(port_EDA$G_average[port$higher=="yes"],)
p<-ggplot(port_EDA, aes(x=G_average, fill=higher, color=higher)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_higher, color=higher),linetype="dashed")
p

#internet

table(port_EDA$internet)

port_EDA$G_average_mean_internet[port$internet=="no"] <- mean(port_EDA$G_average[port$internet=="no"],)
port_EDA$G_average_mean_internet[port$internet=="yes"] <- mean(port_EDA$G_average[port$internet=="yes"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=internet, color=internet)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_internet, color=internet),linetype="dashed")
p

#romantic

table(port_EDA$romantic)

port_EDA$G_average_mean_romantic[port$romantic=="no"] <- mean(port_EDA$G_average[port$romantic=="no"],)
port_EDA$G_average_mean_romantic[port$romantic=="yes"] <- mean(port_EDA$G_average[port$romantic=="yes"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=romantic, color=romantic)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_romantic, color=romantic),linetype="dashed")
p


#famrel

table(port_EDA$famrel)


port_EDA$famrel <- as.character(port_EDA$famrel)
p<-ggplot(port_EDA, aes(x=G_average, y=famrel)) +
  geom_violin(aes(fill=famrel, color=famrel),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#freetime

table(port_EDA$freetime)


port_EDA$freetime <- as.character(port_EDA$freetime)
p<-ggplot(port_EDA, aes(x=G_average, y=freetime)) +
  geom_violin(aes(fill=freetime, color=freetime),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#goout
table(port_EDA$goout)


port_EDA$goout <- as.character(port_EDA$goout)
p<-ggplot(port_EDA, aes(x=G_average, y=goout)) +
  geom_violin(aes(fill=goout, color=goout),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#dalc
table(port_EDA$Dalc)

port_EDA$Dalc <- as.character(port_EDA$Dalc)
p<-ggplot(port_EDA, aes(x=G_average, y=Dalc)) +
  geom_violin(aes(fill=Dalc, color=Dalc),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#walc
table(port_EDA$Walc)

port_EDA$Walc <- as.character(port_EDA$Walc)
p<-ggplot(port_EDA, aes(x=G_average, y=Walc)) +
  geom_violin(aes(fill=Walc, color=Walc),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#health

table(port_EDA$health)


port_EDA$health <- as.character(port_EDA$health)
p<-ggplot(port_EDA, aes(x=G_average, y=health)) +
  geom_violin(aes(fill=health, color=health),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

# traveltime
table(port_EDA$traveltime)


port_EDA$traveltime <- as.character(port_EDA$traveltime)
p<-ggplot(port_EDA, aes(x=G_average, y=traveltime)) +
  geom_violin(aes(fill=traveltime, color=traveltime),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

# studytime
table(port_EDA$studytime)
plot(port_EDA$studytime,port_EDA$G_average)

port_EDA$studytime <- as.character(port_EDA$studytime)
p<-ggplot(port_EDA, aes(x=G_average, y=studytime)) +
  geom_violin(aes(fill=studytime, color=studytime),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

# failures
table(port_EDA$failures)
plot(port_EDA$failures,port_EDA$G_average)

port_EDA$failures <- as.character(port_EDA$failures)
p<-ggplot(port_EDA, aes(x=G_average, y=failures)) +
  geom_violin(aes(fill=failures, color=failures),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p


#Modelle

#Vergleichsmetrik

mean(traindata$G_average)

mean.mqa <- mean(
  (testdata$G_average - mean(traindata$G_average))^2
)
mean.mqa # seed 12:6,70  seed 42:7,39 seed 10: 8,31


#Lineare Regression
lm.fit <- lm( 
  formula =  G_average ~ .,  
  data    = traindata
)

####nice to know
summary(lm.fit)
sum.fit <- as.data.frame(summary(lm.fit)$coefficients)

sum.fit.ordered <- sum.fit[order(abs(sum.fit$Estimate)),]
sum.fit.ordered

#Trainingsfehler: seed 12   4,985
#                 seed 41   5,020
#                 seed 100  5,173
train.mqa.lm <- mean(
  (traindata$G_average - predict(lm.fit, newdata=traindata))^2
)

train.mqa.lm


#Testfehler: seed 12    5,309
#            seed 42    5,115
#            seed 100   4,262
test.mqa.lm <- mean(
  (testdata$G_average - predict(lm.fit, newdata=testdata))^2
)

test.mqa.lm



#Skalierung für Lasso und Ridge
#Aufteilen in nummerische und nicht nummerische Daten, skalieren der nummerischen Daten, wieder zusammenführen
port_numeric <- port %>% dplyr::select(where(is.numeric))
port_numeric <- scale(port_numeric)
port_notnumeric <- port %>% dplyr::select_if(negate(is.numeric))
port_scaled <- cbind(port_numeric, port_notnumeric)
port_scaled <- as.data.frame(port_scaled)
port_scaled$G_average <- port$G_average

traindata_scaled <- port_scaled[trainingsrows,]
testdata_scaled <- port_scaled[-trainingsrows,]



#Anpassung des Formats der Trainings und Testdaten, Aufteilung bleibt gleich
X.train <- model.matrix(G_average ~ ., data = traindata_scaled )[,-1] 
y.train <- traindata_scaled$G_average

X.test <- model.matrix(G_average ~ ., data = testdata_scaled )[,-1]  
y.test <- testdata_scaled$G_average


#Ridge Regression

lambda <- 10^seq( from = 3, to = -3, length = 100) #Fallende Folge! vgl. ?glmnet

ridge.cv.out <- cv.glmnet(
  x     = X.train,
  y     = y.train,
  alpha = 0,
  lambda = lambda
)

ridge.cv.out
plot(ridge.cv.out)
ridge.best.lambda <- ridge.cv.out$lambda.min
ridge.best.lambda  #seed 12: 0,93  seed 42 0,93  1,42



ridge.fit <- glmnet(
  x      = X.train,
  y      = y.train,
  alpha  = 0,
  lambda = ridge.best.lambda
)

coef(ridge.fit)


ridge.prediction.train <- predict(
  object = ridge.fit,
  newx   = X.train
)
ridge.prediction.train  

mqa.ridge.train <- mean( (ridge.prediction.train - y.train)^2 )
mqa.ridge.train # seed12 5,09     seed 42 5,15   5,33


ridge.prediction.test <- predict(
  object = ridge.fit,
  newx   = X.test
)
ridge.prediction.test

mqa.ridge.test <- mean( (ridge.prediction.test - y.test)^2 )
mqa.ridge.test #seed 12 5,13    seed 42 4,77   seed 100 4,42

#Lasso Regression

lasso.cv.out <- cv.glmnet(
  x     = X.train,
  y     = y.train,
  alpha = 1,
  lambda = lambda
)

lasso.cv.out
plot(lasso.cv.out)
lasso.best.lambda <- lasso.cv.out$lambda.min
lasso.best.lambda #seed 12 0,1   0,057  0,1

lasso.fit <- glmnet(
  x      = X.train,
  y      = y.train,
  alpha  = 1,
  lambda = lasso.best.lambda
)

coef(lasso.fit)


lasso.prediction.train <- predict(
  object = lasso.fit, 
  newx   = X.train
)

mqa.lasso.train <- mean( (lasso.prediction.train - y.train)^2 )
mqa.lasso.train #seed12 5,20   seed 42 5,11   seed 100 5,4



lasso.prediction.test <- predict(
  object = lasso.fit, 
  newx   = X.test
)

mqa.lasso.test <- mean( (lasso.prediction.test - y.test)^2 )
mqa.lasso.test #seed 12 5,05  seed 42 4,96   4,47


sum.fit <- coef(lasso.fit)
typeof(sum.fit)
sum.fit.ordered <- sum.fit[order(abs(sum.fit$s0)),]
sum.fit.ordered


#Polynominale Regression

#Versuch 1: mit allen Variablen

maxdegree <- 2
cv.errors          <- rep(0,maxdegree)
glm.polynomial.fit <- vector(mode="list", length=8)

colnames <- toString(colnames(port))
formula <- paste('G_average ~ poly(', colnames, ',degree=i, raw=TRUE)')

for (i in 1:maxdegree){
  
  glm.polynomial.fit[[i]] <- glm(   
    formula =  formula,
    data    = traindata
  )
  
  cv.error <- cv.glm(
    data    = traindata,
    glmfit  = glm.polynomial.fit[[i]],
    K       = 5
  )
  
  cv.errors[[i]] <- cv.error$delta[1]  
}

##Fehler: kann Vektor der Größe 8.0 GB nicht allozieren


#Versuch 2: relevante Varaiblen bei lienarer Regression

#Manuelles erstellen von Dummy-Variablen, da Poly-function sonst die kategorialen Variablen nicht annimmt
port_dummies <- dummy_cols(port)
traindata <- port_dummies[trainingsrows,]
testdata <- port_dummies[-trainingsrows,]

maxdegree <- 4
cv.errors          <- rep(0,maxdegree)
glm.polynomial.fit <- vector(mode="list", length=4)

#Cross Validation zur Bestimmung von i
for (i in 1:maxdegree){
  
  glm.polynomial.fit[[i]] <- glm(   
    formula =  G_average ~ poly(higher_yes,Fjob_teacher,failures,schoolsup_yes,school_MS, degree=i, raw=TRUE),
    data    = traindata
  )
  
  cv.error <- cv.glm(
    data    = traindata,
    glmfit  = glm.polynomial.fit[[i]],
    K       = 5
  )
  
  cv.errors[[i]] <- cv.error$delta[1]  
}

plot(x=port$G_average, port$age, col="red")

degree <- 1:maxdegree
plot(x = degree, y = cv.errors)
lines(x = degree, y = cv.errors)

poly_fit <- glm(   
  formula =  G_average ~ poly(higher_yes,Fjob_teacher,failures,schoolsup_yes, degree=3, raw=TRUE),
  data    = traindata
)

plot(port$age,port$G_average)


poly_fit

train.mqa.lm <- mean(
  (traindata$G_average - predict(poly_fit, newdata=traindata))^2
)

train.mqa.lm


test.mqa.lm <- mean(
  (testdata$G_average - predict(poly_fit, newdata=testdata))^2
)

test.mqa.lm


#Regression Tree

#Wieder normaler Datensatz ohne Dummies
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]

default.model <- rpart(formula = G_average ~ ., data = traindata, method="anova",control=rpart.control(cp=0))
summary(default.model)
rpart.plot(default.model)
text(default.model)

cptable <- printcp(default.model)
mincp <- cptable[which.min(cptable[,"xerror"]),"CP"]
plotcp(default.model)



traindata$pred <- predict(default.model, traindata)
testdata$pred <- predict(default.model, testdata)

mean( 
  ( traindata$G_average - traindata$pred)^2
)
mean( 
  ( testdata$G_average - testdata$pred)^2
)

default.model_pruned <- prune(default.model, cp = mincp)
rpart.plot(default.model_pruned)
text(default.model_pruned)

traindata$pred <- predict(default.model_pruned, traindata)
testdata$pred <- predict(default.model_pruned, testdata)

mean( 
  ( traindata$G_average - traindata$pred)^2
)
mean( 
  ( testdata$G_average - testdata$pred)^2
)


