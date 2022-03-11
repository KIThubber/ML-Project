#Einlesen
# Leon
math <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-mat.csv", sep = ",", header=TRUE)
port <- read.table(file="C:/Users/Z00481XT/Desktop/archive/student-por.csv", sep = ",", header=TRUE)
# Jan
math <- read.table(file="C:/Users/Jan/OneDrive/Dokumente/Studium/4_Semester/Data Exploration/Datenset/student-mat.csv", sep = ",", header=TRUE)
port <- read.table(file="C:/Users/Jan/OneDrive/Dokumente/Studium/4_Semester/Data Exploration/Datenset/student-por.csv", sep = ",", header=TRUE)
# Yannick
port <- read.table(file="C:/Temp/student-por.csv", sep = ",", header=TRUE)
math <- read.table(file="C:/Temp/student-mat.csv", sep = ",", header=TRUE)

#Berechnung der Durchschnittsnote
port$G_average <- (port$G1 + port$G2 + port$G3)/3
port <- port %>% select(-G1,-G2,-G3)
nrow(port)

#Aufteilung in Trainings und Testdaten 80%
set.seed(42) # Zufallsparameter auf 42


trainingsrows <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]

#Aufteilung in Trainings und Testdaten 50%
set.seed(42) # Zufallsparameter auf 42


trainingsrows <- sample(nrow(port), nrow(port)*0.5)    # 80% der Gesamtdaten als Trainingsdaten
traindata <- port[trainingsrows,]
testdata <- port[-trainingsrows,]

##############Lasso und Ridge DAtenvvorhbereitung

X.train <- model.matrix(G_average ~ ., data = traindata ) [,-1] 
y.train <- traindata$G_average

X.test <- model.matrix(G_average ~ ., data = testdata )[,-1]  
y.test <- testdata$G_average
