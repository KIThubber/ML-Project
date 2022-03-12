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

#Korrelationsmatrix
cordata <- port %>% dplyr::select(where(is.numeric)) #Filtern der nummerischen Variablen
cordatamatrix <- cor(cordata)#Erstellen einer Korrelationsmatrix
corrplot(cordatamatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

##### alle kategorialen variablen: school, sex, address, famsize, pstatus, medu, fedu, mjob, fjob, reson, guardian, schoolsup, famsup, paid, activities, nursey, higher, internet, romantic

#######G_Average gegen jede Zielvaraible Grafik######################################
port_EDA <- port

# nur 2 var
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

########EDA Geschlecht
View(port_EDA)
table(port_EDA$sex)

port_EDA$G_average_mean_sex[port$sex=="M"] <- mean(port_EDA$G_average[port$sex=="M"],)
port_EDA$G_average_mean_sex[port$sex=="F"] <- mean(port_EDA$G_average[port$sex=="F"],)


p<-ggplot(port_EDA, aes(x=G_average, fill=sex, color=sex)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_sex, color=sex),linetype="dashed")
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
port_EDA$G_average_mean_famsize[port$famsize=="GT3"] <- mean(port_EDA$G_average[port$famsize=="GT3"],)
port_EDA$G_average_mean_famsize[port$famsize=="LE3"] <- mean(port_EDA$G_average[port$famsize=="LE3"],)


p<-ggplot(port_EDA, aes(x=G_average, fill=famsize, color=famsize)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_famsize, color=famsize),linetype="dashed")
p
## pstatus
# Problem ?
table(port_EDA$Pstatus)

port_EDA$G_average_mean_Pstatus[port$Pstatus=="A"] <- mean(port_EDA$G_average[port$Pstatus=="A"],)
port_EDA$G_average_mean_Pstatus[port$Pstatus=="T"] <- mean(port_EDA$G_average[port$Pstatus=="T"],)

p<-ggplot(port_EDA, aes(x=G_average, y=Pstatus)) +
  geom_violin(aes(fill=Pstatus, color=Pstatus),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

p<-ggplot(port_EDA, aes(x=G_average, fill=Pstatus, color=Pstatus)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_Pstatus, color=Pstatus),linetype="dashed")
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

## fedu
# Problem
table(port_EDA$Fedu)

port_EDA$G_average_mean_Fedu[port$Fedu=="0"] <- mean(port_EDA$G_average[port$Fedu=="0"],)
port_EDA$G_average_mean_Fedu[port$Fedu=="1"] <- mean(port_EDA$G_average[port$Fedu=="1"],)
port_EDA$G_average_mean_Fedu[port$Fedu=="2"] <- mean(port_EDA$G_average[port$Fedu=="2"],)
port_EDA$G_average_mean_Fedu[port$Fedu=="3"] <- mean(port_EDA$G_average[port$Fedu=="3"],)
port_EDA$G_average_mean_Fedu[port$Fedu=="4"] <- mean(port_EDA$G_average[port$Fedu=="4"],)

port_EDA$Fedu <- as.character(port_EDA$Fedu)
p<-ggplot(port_EDA, aes(x=G_average, y=Fedu)) +
  geom_violin(aes(fill=Fedu, color=Fedu),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

# template mehr als 2 var
######EDA Mjob
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

table(port_EDA$Mjob)

port_EDA$G_average_mean_Mjob[port$Mjob=="at_home"] <- mean(port_EDA$G_average[port$Mjob=="at_home"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="health"] <- mean(port_EDA$G_average[port$Mjob=="health"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="other"] <- mean(port_EDA$G_average[port$Mjob=="other"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="services"] <- mean(port_EDA$G_average[port$Mjob=="services"],)
port_EDA$G_average_mean_Mjob[port$Mjob=="teacher"] <- mean(port_EDA$G_average[port$Mjob=="teacher"],)

p<-ggplot(port_EDA, aes(x=G_average, y=Mjob)) +
  geom_violin(aes(fill=Mjob, color=Mjob),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

sum(table(port_EDA$Mjob))

View(port_EDA)

###fjob, 
table(port_EDA$Fjob)

port_EDA$G_average_mean_Fjob[port$Fjob=="at_home"] <- mean(port_EDA$G_average[port$Fjob=="at_home"],)
port_EDA$G_average_mean_Fjob[port$Fjob=="health"] <- mean(port_EDA$G_average[port$Fjob=="health"],)
port_EDA$G_average_mean_Fjob[port$Fjob=="other"] <- mean(port_EDA$G_average[port$Fjob=="other"],)
port_EDA$G_average_mean_Fjob[port$Fjob=="services"] <- mean(port_EDA$G_average[port$Fjob=="services"],)
port_EDA$G_average_mean_Fjob[port$Fjob=="teacher"] <- mean(port_EDA$G_average[port$Fjob=="teacher"],)

p<-ggplot(port_EDA, aes(x=G_average, y=Fjob)) +
  geom_violin(aes(fill=Fjob, color=Fjob),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p


##reason 
table(port_EDA$reason)

port_EDA$G_average_mean_reason[port$reason=="course"] <- mean(port_EDA$G_average[port$reason=="course"],)
port_EDA$G_average_mean_reason[port$reason=="home"] <- mean(port_EDA$G_average[port$reason=="home"],)
port_EDA$G_average_mean_reason[port$reason=="other"] <- mean(port_EDA$G_average[port$reason=="other"],)
port_EDA$G_average_mean_reason[port$reason=="reputation"] <- mean(port_EDA$G_average[port$reason=="reputation"],)

p<-ggplot(port_EDA, aes(x=G_average, y=reason)) +
  geom_violin(aes(fill=reason, color=reason),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#guardian
table(port_EDA$guardian)

port_EDA$G_average_mean_guardian[port$guardian=="father"] <- mean(port_EDA$G_average[port$guardian=="father"],)
port_EDA$G_average_mean_guardian[port$guardian=="mother"] <- mean(port_EDA$G_average[port$guardian=="mother"],)
port_EDA$G_average_mean_guardian[port$guardian=="other"] <- mean(port_EDA$G_average[port$guardian=="other"],)


p<-ggplot(port_EDA, aes(x=G_average, y=guardian)) +
  geom_violin(aes(fill=guardian, color=guardian),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

#schoolsup
table(port_EDA$schoolsup)

port_EDA$G_average_mean_schoolsup[port$schoolsup=="no"] <- mean(port_EDA$G_average[port$schoolsup=="no"],)
port_EDA$G_average_mean_schoolsup[port$schoolsup=="yes"] <- mean(port_EDA$G_average[port$schoolsup=="yes"],)

p<-ggplot(port_EDA, aes(x=G_average, y=schoolsup)) +
  geom_violin(aes(fill=schoolsup, color=schoolsup),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

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

p<-ggplot(port_EDA, aes(x=G_average, y=famsup)) +
  geom_violin(aes(fill=famsup, color=famsup),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

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

p<-ggplot(port_EDA, aes(x=G_average, y=paid)) +
  geom_violin(aes(fill=paid, color=paid),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

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

p<-ggplot(port_EDA, aes(x=G_average, y=activities)) +
  geom_violin(aes(fill=activities, color=activities),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

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

p<-ggplot(port_EDA, aes(x=G_average, y=nursery)) +
  geom_violin(aes(fill=nursery, color=nursery),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

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

p<-ggplot(port_EDA, aes(x=G_average, y=higher)) +
  geom_violin(aes(fill=higher, color=higher),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

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

p<-ggplot(port_EDA, aes(x=G_average, y=internet)) +
  geom_violin(aes(fill=internet, color=internet),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

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

p<-ggplot(port_EDA, aes(x=G_average, y=romantic)) +
  geom_violin(aes(fill=romantic, color=romantic),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p

p<-ggplot(port_EDA, aes(x=G_average, fill=romantic, color=romantic)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.3)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  geom_density(alpha=0.2)+
  geom_vline(data=port_EDA, aes(xintercept=G_average_mean_romantic, color=romantic),linetype="dashed")
p

###############age
#Wenig datenpunkte bei age 21 und 22, nur jew 1 und 2 Personen
table(port_EDA$age)


port_EDA$age <- as.character(port_EDA$age)
p<-ggplot(port_EDA[port_EDA$age<=19,], aes(x=G_average, y=age)) +
  geom_violin(aes(fill=age, color=age),position="identity", alpha=0.8)+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20))+
  stat_summary(fun.data="mean_sdl",geom="crossbar", width=0.05 )
p


