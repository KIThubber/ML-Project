library(boot) #cv.glm()
library(tree) #Entscheidungsbaum


math <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-mat.csv", sep = ",", header=TRUE)
port <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-por.csv", sep = ",", header=TRUE)




View(port)

port$G_average <- (port$G1 + port$G2 + port$G3)/3

port <- port %>% select(-G1,-G2,-G3)


set.seed(42) # Zufallsparameter auf 42


trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]



#############################
# polynominale Regression, Probleme bei vielen variablen und hohen Graden 'Fehler: kann Vektro der Größe 8GB nicht allozieren'
maxdegree <- 3
cv.errors          <- rep(0,maxdegree)
glm.polynomial.fit <- vector(mode="list", length=8)


colnames <- toString(colnames(port))
formula <- paste('G_average ~ poly(', colnames, ',degree=i, raw=TRUE)')


for (i in 1:maxdegree){
  
  glm.polynomial.fit[[i]] <- glm(   
    formula = formula,  
    data    = traindata
  )
  
  cv.error <- cv.glm(
    data    = traindata,
    glmfit  = glm.polynomial.fit[[i]],
    K       = 10
  )
  
  cv.errors[[i]] <- cv.error$delta[1]  
}
plot(x=port$G_average, port$age, col="red")

degree <- 1:maxdegree
plot(x = degree, y = cv.errors)
lines(x = degree, y = cv.errors)
###################


#10-Fold-Cross Validation Linear, VS Lasso, VS Decision Tree


lm.fit <- lm( 
  formula =  G_average ~ .,  
  data    = traindata
)

cv.error <- cv.glm(
  data    = traindata,
  glmfit  = lm.fit,
  K       = 10
)


tree.fit <- tree(
  formula = G_average ~ .,
  data    = traindata
)


summary(tree.fit)

range <- 0:1
plot(range, cv.error)

cv.error

#data cleaning
#normalisierung








cordata <- port %>% dplyr::select(where(is.numeric)) #Filtern der nummerischen Variablen
cordatamatrix <- cor(cordata)#Erstellen einer Korrelationsmatrix
corrplot(cordatamatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
cor(cordata[-14], cordata$G_average)
order(cork)

