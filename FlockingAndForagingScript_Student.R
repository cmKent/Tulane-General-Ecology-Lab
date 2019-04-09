

# Load and organize data --------------------------------------------------

Data<-  #load in the data

  
#Summary Statistics  
library(tidyr);library(dplyr) #load in the packages we need to pipe
SummaryData<-Data %>% #make an object called SummaryData by using the object Data
  group_by(Trial,Day) %>% #group data by day, within trial
  summarise(MeanForagingTime=mean(ForagingTime,na.rm=T), #cacluate the average foraging time
            StandardDeviation=sqrt(var(ForagingTime,na.rm=T)), #standard deviation of foraging time
            N=length(unique(Group)), #number of individuals
            NStarved=sum(CauseOfDeath=='s'), #number starved
            NDepredated=sum(CauseOfDeath=='h' | CauseOfDeath=='g')) #number depredated
SummaryData<-as.data.frame(SummaryData) #Turn back into a data frame

SummaryData$NSurvived<-
SummaryData$PStarved<-
SummaryData$PDepredated<-
SummaryData$PSurvived<-
  
  
#Limit the days of the data
SummaryData<-subset.data.frame()

#Seperate data into trials
Trial1<-
Trial2<-
Trial3<-
Trial4<-
  

# Making plots ------------------------------------------------------------

#Plot average foraging time
plot(MeanForagingTime~Day,data=Trial1,type='o',ylim=c(0,60),ylab='Foraging Time (s)')
lines(MeanForagingTime~Day,data=Trial2,type='o',col='blue')
lines(MeanForagingTime~Day,data=Trial3,type='o',col='red')
lines(MeanForagingTime~Day,data=Trial4,type='o',col='green')
legend('topleft',legend=c('Trial1','Trial2','Trial3','Trial4'),lty=rep(1,4),pch=rep(1,4),col=c('black','blue','red','green'))

#Plot percent starved

#Plot percent depredated

#Plot percent survived


# Assignemnt --------------------------------------------------------------

#T-test function
TTest<-function(m1,m2,SD1,SD2,N1,N2){
  t<-abs((m1-m2)/sqrt(SD1^2/N2+SD2^2/N1))
  df<-N1+N2-2
  p<-2*pt(t,df,lower.tail=F)
  Res<-data.frame(t,df,p)
  return(Res)
}

#Question 1: What was the effect of depleting resources on solitary foraging time?

#Question 2: What is the effect of competition in foraging time?
  
#Question 3: What was the effect of depredation on forager survivorship?
  
#Question 4: Is flocking beneficial in terms of depredation?
  
#Question 5: Is flocking beneficial overall?

