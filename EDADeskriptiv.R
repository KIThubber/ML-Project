#basic statistics
summary(port$G_average)
hist(port$G_average)
sd(port$G_average)
var(port$G_average)

#"Modell"mit MQA als Vergleic hwas unsere Modelle toppen müssen
mean.mqa <- mean(
  (testdata$G_average - mean(traindata$G_average))^2
)
mean.mqa

#hi

#Correlationsmatrix
cordata <- port %>% dplyr::select(where(is.numeric)) #Filtern der nummerischen Variablen
cordatamatrix <- cor(cordata)#Erstellen einer Korrelationsmatrix
corrplot(cordatamatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#######G_Average gegen jede Zielvaraible Grafik######################################
port_EDA <- port

########EDA Geschlecht
table(port_EDA$sex)

port_EDA$G_average_mean_sex[port$sex=="M"] <- mean(port_EDA$G_average[port$sex=="M"],)
port_EDA$G_average_mean_sex[port$sex=="F"] <- mean(port_EDA$G_average[port$sex=="F"],)
View(port_EDA)

p<-ggplot(port_EDA, aes(x=G_average, fill=sex, color=sex)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_sex, color=sex),linetype="dashed")
p

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


###########EDA address

table(port_EDA$address)
port_EDA$G_average_mean_address[port$address=="U"] <- mean(port_EDA$G_average[port$address=="U"],)
port_EDA$G_average_mean_address[port$address=="R"] <- mean(port_EDA$G_average[port$address=="R"],)


p<-ggplot(port_EDA, aes(x=G_average, fill=address, color=address)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.4)+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_address, color=address),linetype="dashed")+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))
p





#######EDA famsize

table(port_EDA$famsize)






######EDA Mjob
table(port_EDA$Mjob)

port_EDA$G_average_mean_Mjob[port$Mjob=="at_home"] <- mean(port_EDA$G_average[port$Mjob=="at_home"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="health"] <- mean(port_EDA$G_average[port$Mjob=="health"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="other"] <- mean(port_EDA$G_average[port$Mjob=="other"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="services"] <- mean(port_EDA$G_average[port$Mjob=="services"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="teacher"] <- mean(port_EDA$G_average[port$Mjob=="teacher"],)

p<-ggplot(port_EDA, aes(x=G_average, fill=Mjob, color=Mjob)) +
  geom_density(position="identity", alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_Mjob, color=Mjob),linetype="dashed")+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))
p


sum(table(port_EDA$Mjob))


##### allle kategorialen variablen: school, sex, address, famsize, pstatus, medu, fedu, mjob, fjob, reson, guardian, schoolsup, famsup, paid, activities, nursey, higher, internet, romantic

