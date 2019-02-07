
# Load data and caluclate life tables -------------------------------------

# Load and organize data
Nola<-read.csv('data/Cemetery_Partial_NOLA_Data.csv')

NolaF<-subset.data.frame(Nola,Nola$Sex=='Female') #grab all of the females
NolaM<-subset.data.frame(Nola,Nola$Sex=='Male') #and grab all the males

#Paramiterize the life tables
#calculate nx
NolaF$nx<-rep(NA) #create an empty placeholder for the data
NolaF$nx[1]<-sum(NolaF$dx) # the first value is easy
for(i in 2:(nrow(NolaF))){ #loop over each row
  NolaF$nx[i]<-NolaF$nx[i-1]-NolaF$dx[i-1] #calculate nx
}

NolaM$nx<-rep(NA) #create an empty placeholder for the data
NolaM$nx[1]<-sum(NolaM$dx) # the first value is easy
for(i in 2:(nrow(NolaM))){ #loop over each row
  NolaM$nx[i]<-NolaM$nx[i-1]-NolaM$dx[i-1] #calculate nx
}

#calculate lx and qx
NolaF$lx<-NolaF$nx/sum(NolaF$dx)  #insert the formula to calculate lx
NolaM$lx<-NolaM$nx/sum(NolaM$dx)

NolaF$qx<-NolaF$dx/NolaF$nx  #insert the formula for qx
NolaM$qx<-NolaM$dx/NolaM$nx

#Check how they turned out
NolaF
NolaM


# Generate curves ---------------------------------------------------------
#Suvivorship curve
plot(lx~AgeClass,data=NolaM, #plot the data
     type='o',col='blue', #make it be blue connected points
     ylim=c(0.01,1),log='y', #log transform the y axis
     ylab='Survivalship (lx)',xlab='Age class') #change the axis labels
points(lx~AgeClass,data=NolaF,type='o',col='red') #plot females
legend('bottomleft',legend=c('Male','Female'),lty=1,pch=1,col=c('blue','red')) #add legend

#Mortality curve
plot(qx~AgeClass,data=NolaM,type='o',col='blue',xlab='Age class',ylab='Mortality (qx)')
points(qx~AgeClass,data=NolaF,type='o',col='red')
legend('topleft',legend=c('Male','Female'),lty=1,pch=1,col=c('blue','red'))

# Statistcal test ---------------------------------------------------------

#First creat a vector with our new age categories
Bins<-c(0,20,40,60,199) 

#Next is the function for putting the data into new bins

Rebin<-function(NewBin,OldBin,Freq){
  NewFreq<-rep(NA,length(NewBin)) #create a placehold for the new frequencies
  for(i in 1:(length(NewBin)-1)){ #loop over each bin of the old data
    sub<-subset(Freq,OldBin>=NewBin[i] & OldBin<NewBin[i+1]) #decide wheather it gets added
    NewFreq[i]<-sum(sub) #add the ones that met the rule
  }
  return(NewFreq)
}

#Now, use the function to create a table of observed frequencies

Observed<-data.frame(Female=Rebin(Bins,NolaF$AgeClass,NolaF$dx), 
                     Male=Rebin(Bins,NolaM$AgeClass,NolaM$dx))
rownames(Observed)<-Bins
Observed<-Observed[-5,]

#now run the test
Chisq=chisq.test(Observed)
