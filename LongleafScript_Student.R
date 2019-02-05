#Student script for the longleaf pine diversity lab


# Load and organize -----------------------------------------------------

Data<-read.csv('Data/Longleaf_Data.csv') #load in the data
head(Data) #look at the first few rows
tail(Data) #look at the last few rows
str(Data) #look at the structure of the data
summary(Data) #look at the summaries of each column

LL<-subset.data.frame(Data,Data$Habitat=='LL') #create a subset of data for longleaf pine
BH<-  #Enter code here

# Preston Plots -----------------------------------------------------------

#Create bins
Bins<-c(0,1,2,4,8,16,32,64,128,256,512,1024,2048) # a vector containing our abundance categories

#create empty data frame
PrestonData<-data.frame(Min=Bins[-length(Bins)],Max=Bins[-1]) #Create a data frame with mins and maxes matching the bins above
PrestonData$LL<-rep(NA) #Add some empty data for the longleaf habitat
PrestonData$BH<-rep(NA)#Add some emtpy data for the bayhead habitat
PrestonData$Name<-paste(PrestonData$Min,PrestonData$Max,sep='-') #Add a name for each bin
PrestonData

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
AccumData

#Make graph
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
points() #add the points from bayhead samp
legend() #add a legend

# Shannon's diversity -----------------------------------------------------

#calculate pi
LL_pi<- #calulate p(i) for each species (i)
BH_pi<- #do the same for bayhead
  
#Calculate H'
LL_H<- #calculate Shannon's diversity for LL
BH_H<- #do the same for bayhead
LL_H
BH_H

#Cacluate eveness

LL_E<- #calculate Shannon's evenness for LL
BH_E<- #do the same for bayhead
LL_E
BH_E

#calculate variance
LL_v<- #calculate variance for LL
BH_v<- #do the same for bayhead
LL_v
BH_v

#calculate t
t<-abs() #calculate t
t

#calculate df
df<- #calculate df
df

#calculate p value
p<-2*pt(t,df,lower.tail=F)
p


# Simpson's diversity -----------------------------------------------------

#calculate N and n
LL_N<- #calculate N for longleaf 
LL_n<- #calculate n for longleaf
BH_N<- #do the same for bayhead
BH_n<-

#calculate Ds
LL_Ds<- #calculate Ds for longleaf 
BH_Ds<- #do the same for bayhead
LL_Ds
BH_Ds

#calculate Ed
LL_Ed<- #calculate Ed for longleaf 
BH_Ed<- #do the same for bayhead
LL_Ed
BH_Ed