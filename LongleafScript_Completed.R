#Compleated script for the longleaf pine diversity lab


# Load and organize -----------------------------------------------------

Data<-read.csv('Data/Longleaf_Data.csv') #load in the data
head(Data) #look at the first few rows
tail(Data) #look at the last few rows
str(Data) #look at the structure of the data
summary(Data) #look at the summaries of each column

LL<-subset.data.frame(Data,Data$Habitat=='LL') #create a subset of data for longleaf pine
BH<-subset.data.frame(Data,Data$Habitat=='BH') #Enter code here
  
# Preston Plots -----------------------------------------------------------

#Create bins
Bins<-c(0,1,2,4,8,16,32,64,128,256,512,1024,2048) # a vector containing our abundance categories

#create empty data frame
PrestonData<-data.frame(Min=Bins[-length(Bins)],Max=Bins[-1]) #Create a data frame with mins and maxes matching the bins above
PrestonData$LL<-rep(NA) #Add some empty data for the longleaf habitat
PrestonData$BH<-rep(NA)#Add some emtpy data for the bayhead habitat
PrestonData$Name<-paste(PrestonData$Min,PrestonData$Max,sep='-') #Add a name for each bin
PrestonData

#Make function to count the number of species in each bin
count.bin<-function(X,Min,Max){
  Number<-sum(X>Min & X<Max) #Count the number of cases where X is greater than the min and less than the max
  Number<-Number+sum(X==Min)/2 #Add in the number of cases where X is equal to the minimum devided by two
  Number<-Number+sum(X==Max)/2 #do the same thing for X equals the maximum
  return(Number) #return the total number
}

#Calcualte the number of species in each habitat at each bin
for(r in 1:nrow(PrestonData)){
  PrestonData$LL[r]<-count.bin(X=LL$Count,Min=PrestonData$Min[r],Max = PrestonData$Max[r]) #calculate the number of species in the bin for each row (r) of the data frame
  PrestonData$BH[r]<-count.bin(X=BH$Count,Min=PrestonData$Min[r],Max = PrestonData$Max[r]) #do the same for bayhead
}
PrestonData

#Make the graphs
#Here we make a barplot for the Lonleaf data. Lets break down each step
barplot(height=rbind(PrestonData$LL,PrestonData$BH), #The heights of the bars come form the PrestonData LL and BH columns
        names.arg=PrestonData$Name, #The names for the x axis lables come form PrestonData$Names
        col=c('Blue','Red'), #Set the colors for the bars
        las=2, #Rotate the x axis labels vertically
        beside=T, #place the bars beside eachother (default is stacked)
        xlab='Abundance',#add the x axis name
        ylab='Number of species') #add the y axis name
legend('topright',legend=c('Longleaf','Bayhead'),fill=c('Blue','Red')) #make a legend for the graph

# Species Accumulation Curves ---------------------------------------------


#Make the data frame
AccumData<-data.frame(
  Group=seq(from=1,to=max(max(BH$Group,LL$Group)),by=1), #Lets make a column called group that goes from group 1 to the number of groups we have
  LL=rep(NA),BH=rep(NA)) #put in placeholders for LL and BH
AccumData

#Fill in the data
for(r in 1:nrow(AccumData)){
  AccumData$LL[r]<-sum(LL$Group<=AccumData$Group[r]) #for each row, the value is the number of groups that are equal to or less than the group of intrest
  AccumData$BH[r]<-sum(BH$Group<=AccumData$Group[r]) #do it agaoin for Bayhead
}

#make graph
#first we will make an empty plot
plot(y=c(0,max(c(AccumData$LL,AccumData$BH))), #the y axis should go from 0 to the largest value
     x=c(1,max(AccumData$Group)), #the x axis should go form 1 to the largest value
     type='n', #tell R not to actually plot anything
     ylab='Number of species',xlab='Number of groups') #set x and y axis labels
#now we will ad the points for longleaf pine
points(LL~Group, #Plot the data from LL against the group number
       data=AccumData, #tell R where to get the data
       type='o',# Make it be points connected by lines
       pch=16,col='blue') #set the shape (pch) and collor
points(BH~Group, #Plot the data from LL against the group number
       data=AccumData, #tell R where to get the data
       type='o',# Make it be points connected by lines
       pch=16,col='red') #set the shape (pch) and collor
legend('topleft',legend=c('Longleaf','Bayhead'),lty=1,col=c('blue','red'),pch=16)



# Shannon's diversity -----------------------------------------------------

#calculate pi
LL_pi<-LL$Count/sum(LL$Count)
BH_pi=BH$Count/sum(BH$Count)

#calculate H'
LL_H<-sum(LL_pi*log(LL_pi))*-1 #calculate Shannon's diversity for LL
BH_H<-sum(BH_pi*log(BH_pi))*-1 #do the same for bayhead

#calculate eveness
LL_E<-LL_H/log(nrow(LL)) #calculate Shannon's eveness for LL
BH_E<-BH_H/log(nrow(BH)) #do the same for bayhead

#calculate variance
LL_v<-((sum(LL_pi*(log(LL_pi)^2))-(sum(LL_pi*log(LL_pi))^2))/sum(LL$Count))-((nrow(LL)-1)/(2*(sum(LL$Count)^2))) #calculate variance for LL
BH_v<-((sum(BH_pi*(log(BH_pi)^2))-(sum(BH_pi*log(BH_pi))^2))/sum(BH$Count))-((nrow(BH)-1)/(2*(sum(BH$Count)^2))) #do the same for bayhead

#calculate t
t<-abs(LL_H-BH_H)/(LL_v+BH_v)^(1/2)

#calculate df
df<-((LL_v+BH_v)^2)/(LL_v^2/sum(LL$Count)+BH_v^2/sum(BH$Count))

#calculate p value
p<-2*pt(t,df,lower.tail=F)
p


# Simpson's ---------------------------------------------------------------

#calculate N and n
LL_N<-sum(LL$Count)
LL_n<-LL$Count
BH_N<-sum(BH$Count)
BH_n<-BH$Count

#calculate Ds
LL_Ds<-(LL_N*(LL_N-1))/sum(LL_n*(LL_n-1)) #calculate Ds for longleaf 
BH_Ds<-(BH_N*(BH_N-1))/sum(BH_n*(BH_n-1)) #do the same for bayhead

#calculate Ed
LL_Ed<-LL_Ds/nrow(LL) #calculate Ed for longleaf 
BH_Ed<-BH_Ds/nrow(BH) #do the same for bayhead
