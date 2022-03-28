library(dplyr)
library(rpart)
library("rpart.plot")

port <- read.table(file="C:/Temp/student-por.csv", sep = ",", header=TRUE)
math <- read.table(file="C:/Temp/student-mat.csv", sep = ",", header=TRUE)

port$G_average <- (port$G1 + port$G2 + port$G3)/3
port <- port %>% select(-G1,-G2,-G3)

set.seed(42) # Zufallsparameter auf 42

trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]

default.model <- rpart(formula = G_average ~ ., data = traindata, method="anova",control=rpart.control(cp=0))
summary(default.model)
rpart.plot(default.model)
text(default.model)

printcp(default.model)
plotcp(default.model)



traindata$pred <- predict(default.model, traindata)
testdata$pred <- predict(default.model, testdata)

mean( 
  ( traindata$G_average - traindata$pred)^2
)
mean( 
  ( testdata$G_average - testdata$pred)^2
)

default.model_pruned <- prune(default.model, cp = 0.0157505)
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

