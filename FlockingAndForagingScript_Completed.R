

# Load and organize data --------------------------------------------------

Data<-read.csv('Data/FlockingAndForaging_Data.csv') #use the read.csv() file to load in the data
  
  
#Summary Statistics  
library(tidyr);library(dplyr)
SummaryData<-Data %>%
  group_by(Trial,Day) %>%
  summarise(MeanForagingTime=mean(ForagingTime,na.rm=T),
            StandardDeviation=sqrt(var(ForagingTime,na.rm=T)),
            N=length(unique(Group)),
            NStarved=sum(CauseOfDeath=='s'),
            NDepredated=sum(CauseOfDeath=='h' | CauseOfDeath=='g'))
SummaryData$NSurvived<-SummaryData$N-SummaryData$NStarved-SummaryData$NDepredated
SummaryData$PStarved<-SummaryData$NStarved/SummaryData$N
SummaryData$PDepredated<-SummaryData$NDepredated/SummaryData$N
SummaryData$PSurvived<-SummaryData$NSurvived/SummaryData$N
SummaryData<-as.data.frame(SummaryData)
  
#Limit the days of the data
SummaryData<-subset.data.frame(SummaryData,SummaryData$Day==3 | SummaryData$Day==10 |SummaryData$Day==17)

#Seperate data into trials
Trial1<-subset.data.frame(SummaryData,SummaryData$Trial==1)
Trial2<-subset.data.frame(SummaryData,SummaryData$Trial==2)
Trial3<-subset.data.frame(SummaryData,SummaryData$Trial==3)
Trial4<-subset.data.frame(SummaryData,SummaryData$Trial==4)
  
  
# Making plots ------------------------------------------------------------

#Plot average foraging time
plot(MeanForagingTime~Day,data=Trial1,type='o',ylim=c(0,60),ylab='Foraging Time (s)')
lines(MeanForagingTime~Day,data=Trial2,type='o',col='blue')
lines(MeanForagingTime~Day,data=Trial3,type='o',col='red')
lines(MeanForagingTime~Day,data=Trial4,type='o',col='green')
legend('topleft',legend=c('Trial1','Trial2','Trial3','Trial4'),lty=rep(1,4),pch=rep(1,4),col=c('black','blue','red','green'))

#Plot percent starved
plot(PStarved~Day,data=Trial1,type='o',ylim=c(0,1),ylab='Percent Starved')
lines(PStarved~Day,data=Trial2,type='o',col='blue')
lines(PStarved~Day,data=Trial3,type='o',col='red')
lines(PStarved~Day,data=Trial4,type='o',col='green')
legend('topleft',legend=c('Trial1','Trial2','Trial3','Trial4'),lty=rep(1,4),pch=rep(1,4),col=c('black','blue','red','green'))

#Plot percent depredated
plot(PDepredated~Day,data=Trial1,type='o',ylim=c(0,1),ylab='Percent Depredated')
lines(PDepredated~Day,data=Trial2,type='o',col='blue')
lines(PDepredated~Day,data=Trial3,type='o',col='red')
lines(PDepredated~Day,data=Trial4,type='o',col='green')
legend('topleft',legend=c('Trial1','Trial2','Trial3','Trial4'),lty=rep(1,4),pch=rep(1,4),col=c('black','blue','red','green'))

#Plot percent survived
plot(PSurvived~Day,data=Trial1,type='o',ylim=c(0,1),ylab='Percent Survived')
lines(PSurvived~Day,data=Trial2,type='o',col='blue')
lines(PSurvived~Day,data=Trial3,type='o',col='red')
lines(PSurvived~Day,data=Trial4,type='o',col='green')
legend('bottomleft',legend=c('Trial1','Trial2','Trial3','Trial4'),lty=rep(1,4),pch=rep(1,4),col=c('black','blue','red','green'))

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
Test12<-TTest(Trial1$MeanForagingTime[1],Trial1$MeanForagingTime[2],Trial1$StandardDeviation[1],Trial1$StandardDeviation[2],Trial1$N[1],Trial1$N[2])
Test23<-TTest(Trial1$MeanForagingTime[2],Trial1$MeanForagingTime[3],Trial1$StandardDeviation[2],Trial1$StandardDeviation[3],Trial1$N[2],Trial1$N[3])
Test13<-TTest(Trial1$MeanForagingTime[1],Trial1$MeanForagingTime[3],Trial1$StandardDeviation[1],Trial1$StandardDeviation[3],Trial1$N[1],Trial1$N[3])
#Question 2: What is the effect of competition in foraging time?
Test1<-TTest(Trial1$MeanForagingTime[1],Trial2$MeanForagingTime[1],Trial1$StandardDeviation[1],Trial2$StandardDeviation[1],Trial1$N[1],Trial2$N[1])
Test2<-TTest(Trial1$MeanForagingTime[2],Trial2$MeanForagingTime[2],Trial1$StandardDeviation[2],Trial2$StandardDeviation[2],Trial1$N[2],Trial2$N[2])
Test3<-TTest(Trial1$MeanForagingTime[3],Trial2$MeanForagingTime[3],Trial1$StandardDeviation[3],Trial2$StandardDeviation[3],Trial1$N[3],Trial2$N[3])
#Question 3: What was the effect of depredation on forager survivorship?
Obs<-data.frame(Trial1$NSurvived,Trial3$NSurvived)
Test<-chisq.test(Obs)
#Question 4: Is flocking beneficial in terms of depredation?
Obs<-data.frame(Trial1$NSurvived,Trial3$NSurvived)
Test<-chisq.test(Obs)
#Question 5: Is flocking beneficial overall?
Obs<-data.frame(Trial3$NSurvived,Trial4$NSurvived)
Test<-chisq.test(Obs)
