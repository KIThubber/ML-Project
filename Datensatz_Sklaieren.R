####Erstellen eines Skalierten Datensatzues

#Aufteilen in nummerische und nicht nummerische Daten, skalieren der nummerischen daten, wieder zusammenführen
port_numeric <- port %>% dplyr::select(where(is.numeric))
port_numeric <- scale(port_numeric)
port_notnumeric <- port %>% dplyr::select_if(negate(is.numeric))
port_scaled <- cbind(port_numeric, port_notnumeric)
port_scaled <- as.data.frame(port_scaled)
port_scaled$G_average <- port$G_average

#####Aufteilen Test und Trainignsdaten

set.seed(42)
trainingsrows_scaled <- sample(nrow(port), nrow(port)*0.8)    # 80% der Gesamtdaten als Trainingsdaten
traindata_scaled <- port_scaled[trainingsrows_scaled,]
testdata_scaled <- port_scaled[-trainingsrows_scaled,]