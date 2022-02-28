library(boot) #cv.glm()
library(tree) #Entscheidungsbaum
library(glmnet) 
library(dplyr)
<<<<<<< HEAD
library(rpart)
install.packages("rpart.plot")
library("rpart.plot")
=======
library(corrplot)
>>>>>>> 5d88b560a9dec7bd6058b6f1b2e72085205049b6
install.packages('Rcpp')

# Leon
math <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-mat.csv", sep = ",", header=TRUE)
port <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-por.csv", sep = ",", header=TRUE)
# Jan
math <- read.table(file="C:/Users/Jan/OneDrive/Dokumente/Studium/4_Semester/Data Exploration/Datenset/student-mat.csv", sep = ",", header=TRUE)
port <- read.table(file="C:/Users/Jan/OneDrive/Dokumente/Studium/4_Semester/Data Exploration/Datenset/student-por.csv", sep = ",", header=TRUE)

<<<<<<< HEAD
port <- read.table(file="C:/Temp/student-por.csv", sep = ",", header=TRUE)
math <- read.table(file="C:/Temp/student-mat.csv", sep = ",", header=TRUE)

=======
View(port)
>>>>>>> 5d88b560a9dec7bd6058b6f1b2e72085205049b6
port$G_average <- (port$G1 + port$G2 + port$G3)/3

port <- port %>% select(-G1,-G2,-G3)


set.seed(42) # Zufallsparameter auf 42


trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]

set.seed(42) # Zufallsparameter auf 42


trainingsrows <- sample(nrow(port), nrow(port)*0.5)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]



##########deskriptive statistik

summary(traindata$G_average)
hist(traindata$G_average)
sd(traindata$G_average)
var(traindata$G_average)

mean.mqa <- mean(
  (testdata$G_average - mean(traindata$G_average))^2
)
mean.mqa


cordata <- port %>% dplyr::select(where(is.numeric)) #Filtern der nummerischen Variablen
cordatamatrix <- cor(cordata)#Erstellen einer Korrelationsmatrix
corrplot(cordatamatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
cor(cordata[-14], cordata$G_average)
order(cork)


#############################
# polynominale Regression, Probleme bei vielen variablen und hohen Graden 'Fehler: kann Vektro der Größe 8GB nicht allozieren'
maxdegree <- 3
cv.errors          <- rep(0,maxdegree)
glm.polynomial.fit <- vector(mode="list", length=8)


colnames <- toString(colnames(port))
formula <- paste('G_average ~ poly(', colnames, ',degree=i, raw=TRUE)')

memory.limit(9999999)


for (i in 1:maxdegree){
  
  glm.polynomial.fit[[i]] <- glm(   
    formula = G_average ~ poly(school,guardian,studytime,failures,schoolsup,higher,romantic, degree=i, raw=TRUE),  
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
###################


#10-Fold-Cross Validation Linear, VS Lasso, VS Decision Tree


########### lin-reg ############

lm.fit <- lm( 
  formula =  G_average ~ .,  
  data    = traindata
)

lm.fit

train.mqa.lm <- mean(
  (testdata$G_average - predict(lm.fit, newdata=testdata))^2
)

train.mqa.lm
###############

lm.fit <- glm( 
  formula =  G_average ~ .,  
  data    = traindata
)

lm.fit
summary(lm.fit)


cv.error <- cv.glm(
  data    = traindata,
  glmfit  = lm.fit,
  K       = 10
)

cv.error$delta[1]
#############################
################################################################################
################################################################################



################################################################################
################################################################################

####ggplot2
# alles installieren
install.packages("tidyverse")
# nur ggplot
install.packages("ggplot2")
#
library(ggplot2)

################################################################################
## ggplot2
ggplot(data = port) + 
  geom_point(mapping = aes(x = age , y = absences, color = sex))

ggplot(data = port) + 
  geom_point(mapping = aes(x = freetime , y = absences, color = sex))

ggplot(data = port) + 
  geom_point(mapping = aes(x = freetime , y = studytime, color = "blue"))

# Fehlzeiten, abhängig von Alter und unterteilt in Geschlecht
ggplot(data = port) + 
  geom_point(mapping = aes(x = age, y = absences)) + 
  facet_wrap(~ sex, nrow = 2)
# Fehlzeiten, abhängig von Alter und unterteilt in Geschlecht und Schule
ggplot(data = port) + 
  geom_point(mapping = aes(x = age, y = absences)) + 
  facet_grid(sex ~ school)

################################################################################
## Geometrische Objekte
# Fehlzeiten abh von Alter
ggplot(data = port) + 
  geom_smooth(mapping = aes(x = age, y = port$G_average))
# Fehlzeiten abh von Alter, unterteilt in Geschlecht
ggplot(data = port) + 
  geom_smooth(mapping = aes(x = age, y = port$G_average, linetype = sex))
#
ggplot(data = port) +
  geom_smooth(
    mapping = aes(x = age, y = port$G_average, color = sex),
    show.legend = FALSE
  )
#
ggplot(data = port) + 
  geom_point(mapping = aes(x = age, y = port$G_average)) +
  geom_smooth(mapping = aes(x = age, y = port$G_average))
#
ggplot(data = port, mapping = aes(x = age, y = absences)) + 
  geom_point(mapping = aes(color = sex)) + 
  geom_smooth()
#
ggplot(data = port, mapping = aes(x = age, y = absences)) + 
  geom_point(mapping = aes(color = sex)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
# Filter deklarieren
filter(x, filter, method = c("convolution", "recursive"),
       sides = 2, circular = FALSE, init)

# Test
bar <- ggplot(data = port) + 
  geom_bar(
    mapping = aes(x = freetime, fill = absences), 
    show.legend = FALSE,
    width = 1)+ 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
  
bar + coord_flip()
bar + coord_polar()

################################################################################
################################################################################



####################### treeee ################
n <- nrow(traindata)
set.seed(42)
trainingRows <- sample(n,0.8*n)

TreeTrain    <- traindata[trainingRows,]
TreeTest     <- traindata[-trainingRows,]


tree.fit <- tree(
  formula = G_average ~ .,
  data    = traindata
)
summary(tree.fit)
plot(tree.fit)
text(tree.fit)

default.model <- rpart(formula = G_average ~ ., data = traindata, method="anova",control=rpart.control(minsplit=60, cp=0.001))
cv.Alk <- cv.tree(overfit.model)
plot(
  x    = cv.Alk$size,
  y    = cv.Alk$dev,
  type = "b"
)
cv.Alk
pruned.tree <- prune.tree(
  tree = tree.fit,
  best = 3
)

plot(pruned.tree)
text(pruned.tree)


# Trainingsfehler (MQA)
mean( 
  ( TreeTrain$G_average - predict(pruned.tree,newdata=TreeTrain) )^2
)

# Testfehler (MQA)
mean( 
  ( TreeTest$G_average - predict(pruned.tree,newdata=TreeTest) )^2
)

overfit.model <- rpart(G_average ~ ., data = traindata,
                       maxdepth= 7, minsplit=2,
                       minbucket = 20)
rpart.plot(overfit.model)
printcp(overfit.model)
summary(overfit.model)
plot(overfit.model)
text(overfit.model)
# printcp(fit)
# fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
# prune(fit, cp= 1 )
#########################




############### lasso-reg / ridge reg ############################################################################
X.train <- model.matrix(G_average ~ ., data = traindata ) [,-1] 
y.train <- traindata$G_average

X.test <- model.matrix(G_average ~ ., data = testdata )[,-1]  
y.test <- testdata$G_average


#lineares Modell
lm.fit <- lm(
  formula = G_average ~ .,
  data    = traindata
)

cv.error <- cv.glm(
  data    = traindata,
  glmfit  = lm.fit,
  K       = 10
)

cv.error

lm.fit

summary(lm.fit)

###Trainingsfehler 5,0197
train.mqa.lm <- mean(
  (traindata$G_average - predict(lm.fit, newdata=traindata))^2
)

train.mqa.lm


####Testfehler 5,11

test.mqa.lm <- mean(
  (testdata$G_average - predict(lm.fit, newdata=testdata))^2
)

test.mqa.lm

#RidgeReg
lambda <- 10^seq( from = 5, to = -5, length = 100) #Fallende Folge! vgl. ?glmnet

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


#Lasso Reg

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

summary(lasso.fit)


#######################################################################################################################


cv.port <- cv.tree(tree.fit)
plot(
  x    = cv.BostonHousing$size,
  y    = cv.BostonHousing$dev,
  type = "b"
)




range <- 0:1
plot(range, cv.error)

cv.error

#data cleaning
#normalisierung


####vergleich
mean(traindata$G_average)


mean.mqa <- mean(
  (testdata$G_average - mean(traindata$G_average))^2
)
mean.mqa






