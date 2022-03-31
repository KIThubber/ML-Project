library(boot) #cv.glm()
library(glmnet) 
library(dplyr)
library(rpart)
library("rpart.plot")
library(corrplot)
library(Hmisc)
library(tidyverse)
library(ggplot2)

# Install the required package
install.packages("fastDummies")

# Load the library
library(fastDummies)


math <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-mat.csv", sep = ",", header=TRUE)
port <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-por.csv", sep = ",", header=TRUE)

port$G_average <- (port$G1 + port$G2 + port$G3)/3
port <- port %>% select(-G1,-G2,-G3)





#################################################################################################seed40
set.seed(12) # Zufallsparameter auf 12
trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]


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
#                 seed 41   
#                 seed 100  
train.mqa.lm <- mean(
  (traindata$G_average - predict(lm.fit, newdata=traindata))^2
)

train.mqa.lm


#Testfehler: seed 12    5,309
#            seed 42    
#            seed 100   
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
ridge.best.lambda

ridge.fit <- glmnet(
  x      = X.train,
  y      = y.train,
  alpha  = 0,
  lambda = ridge.best.lambda
)

coef(ridge.fit)

#train 5,13
ridge.prediction.train <- predict(
  object = ridge.fit,
  newx   = X.train
)
ridge.prediction.train

mqa.ridge.train <- mean( (ridge.prediction.train - y.train)^2 )
mqa.ridge.train 

#test 4,768
ridge.prediction.test <- predict(
  object = ridge.fit,
  newx   = X.test
)
ridge.prediction.test

mqa.ridge.test <- mean( (ridge.prediction.test - y.test)^2 )
mqa.ridge.test 

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
lasso.best.lambda

lasso.fit <- glmnet(
  x      = X.train,
  y      = y.train,
  alpha  = 1,
  lambda = lasso.best.lambda
)

coef(lasso.fit)

#trainingsfehler5,1
lasso.prediction.train <- predict(
  object = lasso.fit, 
  newx   = X.train
)

mqa.lasso.train <- mean( (lasso.prediction.train - y.train)^2 )
mqa.lasso.train


#testfehler 4,96
lasso.prediction.test <- predict(
  object = lasso.fit, 
  newx   = X.test
)

mqa.lasso.test <- mean( (lasso.prediction.test - y.test)^2 )
mqa.lasso.test


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
    formula =  G_average ~ poly(higher_yes,Fjob_teacher,failures,schoolsup_yes, degree=i, raw=TRUE),
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
  formula =  G_average ~ poly(higher_yes,Fjob_teacher,failures,schoolsup_yes, degree=2, raw=TRUE),
  data    = traindata
)

train.mqa.lm <- mean(
  (traindata$G_average - predict(poly_fit, newdata=traindata))^2
)

train.mqa.lm


test.mqa.lm <- mean(
  (testdata$G_average - predict(poly_fit, newdata=testdata))^2
)

test.mqa.lm












