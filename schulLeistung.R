d1 <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-mat.csv", sep = ",", header=TRUE)
d2 <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-por.csv", sep = ",", header=TRUE)

d1 <- read.table(file="C:/Temp/student-por.csv", sep = ",", header=TRUE)
d2 <- read.table(file="C:/Temp/student-mat.csv", sep = ",", header=TRUE)

data <- merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(data)) # 382 students

View(data)
View(d1)


library("correlation")
library("Hmisc")
library(dplyr)
library(corrplot)


#correlation
cordata <- data2 %>% dplyr::select(where(is.numeric)) #Filtern der nummerischen Variablen
cordata <- cor(cordata)#Erstellen einer Korrelationsmatrix
corrplot(cordata, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Schulischer Erfolg Fragestellung
# Gegeben sind G1, G2 und G3 für 2 verscheidene Fächer -> Mittelwert um eine Varaible zu erhalten

data$Noten <- (data$G1.x + data$G1.y + data$G2.x + data$G2.y + data$G3.x + data$G3.y)/6
data$Guardian <- (data$guardian.x)
data$guardian.x == data$guardian.y
data$traveltime.x == data$traveltime.y

data$studytime.y == data$studytime.x


data$

data
data2 <- data %>% select(-G1.x,-G1.y,-G2.x,-G2.y,-G3.x,-G3.y)




#Train/Test
set.seed(42) # Zufallsparameter auf 42


trainingsrows <- sample(nrow(data2), nrow(data2)*0.8)    # 80% der Gesamtdaten als Trainingsdaten

traindata <- data2[trainingsrows,]
testdata <- data2[-trainingsrows,]


#Lin Regression

data2$Dalc.y <- as.double(data2$Dalc.y)

lm.fit <- lm( 
  formula =  Noten ~ Fedu,  
  data    = data2
)

lm.fit

train.mqa.lm <- mean(
  (testdata$Noten - predict(lm.fit, newdata=testdata))^2
)

predict(lm.fit, newdata=testdata)

train.mqa.lm


mean.mqa <- mean(
  (testdata$Noten - mean(traindata$Noten))^2
)

mean.mqa



is.null(data)







