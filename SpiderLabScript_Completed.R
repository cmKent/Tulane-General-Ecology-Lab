#Spider Lab Analysis
##This is a script to run the data analyis for the Spider Niche Partioning Lab
##Before running this script, you should have already cleaned the data in excell

# Load  -----------------------------------------------------

Data <- read.csv('Data/Spider_Data.csv') #Import the spider data into R as a data frame and name it Data

#Before we do any analysis, we should look at our data frame to make sure it imported correctly

head(Data) #Use the head function to look at the first few rows of Data
tail(Data) #Use the tail function to look at the last few rows of Data.
#Why is it good to look at the last few rows?

str(Data) #use the structure function to look at what kind of data was generated

#Does the data look like it all loaded correctly?

# Cleaning up data ----------------------------------------------------------------

#So we aren't really intestested in the web diameter, but really want to use them to find the web area.
##How could we do this? Fill in the line below 

Data$Web.Area <-pi*((Data$Web.Diam.1+Data$Web.Diam.2)/2/2)^2 #What formula would you use to calculate web area?

#Also, there is a chance that some data for web angle could have entered the obtuse, instead of accute angle. Lets correct this.
Data$Orientation[Data$Orientation>90]=180-Data$Orientation[Data$Orientation>90]

#If we use the summary() function we can see how many individuals there are of each species
summary(Data$Spider.ID)
#For our purpuses, lets use only the species where we measured more than 10 indiviudals

keep_species<-names(summary(Data$Spider.ID)[summary(Data$Spider.ID)>=10]) #Create a vector of spider species with 10 or more individuals
Data<-subset.data.frame(Data,Data$Spider.ID%in%keep_species)

summary(Data$Spider.ID) #notice how the Spider.ID vector still contains the levels for the removed species
Data$Spider.ID<-droplevels(Data$Spider.ID) #use droplevels() to remove these
summary(Data$Spider.ID)
# Running the t test -------------------------------------------------------

#To help organize ourselves, lets split the data set up into the three species
GC<-subset.data.frame(Data,Data$Spider.ID=='Gasteracantha cancriformis')
LV<-subset.data.frame(Data,Data$Spider.ID=='Leucauge venusta')
NC<-subset.data.frame(Data,Data$Spider.ID=='Nephila clavipes')
#Now we will run some t tests
##Remember! a t test compares means between two gorups

#Lets begin by creating a function to run a t-test
#Fill in what is missing below


TTest<-function(v1,v2){
  m1<-mean(v1,na.rm=T) #calcuate the mean for the first species
  m2<-mean(v2,na.rm=T) #calculating the mean for the second speces
  s1<-var(v1,na.rm=T) #calculate variance for first speices
  s2<-var(v2,na.rm=T) #variance for second species
  n1<-length(v1)-sum(is.na(v1)) #find the number of samples for first species
  #What does the sum(is.na(v)) part of the equation do?
  n2<-length(v2)-sum(is.na(v2)) #find the number of samples for second species
  
  t<-abs((m1-m2)/(sqrt(s1/n1+s2/n2))) #cacluate the t statisitic)
  
  df<-n1+n2-2 #calcualate the degrees of freedom
  
  p<-2*pt(t,df,lower.tail=F) #find the p value
  Results<-data.frame(t,df,p) #organize the results
  return(Results) #return the results
}

#Now lets compare the web hieghts for two of the species
#Comparing GC to LV
TTest(GC$Web.Height,LV$Web.Height)
#Be sure to look at the output from this function


#Now run the test again for the other species pairs
TTest(GC$Web.Height,NC$Web.Height)
TTest(LV$Web.Height,NC$Web.Height)


### For your lab report, you should do this same thing, running all pairwise t-tests for 3 other web variables
##Do this now

#For body length
TTest(GC$Body.Length,LV$Body.Length)
TTest(GC$Body.Length,NC$Body.Length)
TTest(LV$Body.Length,NC$Body.Length)

#For Strand Density
TTest(GC$Strand.density,LV$Strand.density)
TTest(GC$Strand.density,NC$Strand.density)
TTest(LV$Strand.density,NC$Strand.density)

#For Number of Radii
TTest(GC$Number.of.Radii,LV$Number.of.Radii)
TTest(GC$Number.of.Radii,NC$Number.of.Radii)
TTest(LV$Number.of.Radii,NC$Number.of.Radii)

#for Orientation
TTest(GC$Orientation,LV$Orientation)
TTest(GC$Orientation,NC$Orientation)
TTest(LV$Orientation,NC$Orientation)

#For Web Area

TTest(GC$Web.Area,LV$Web.Area)
TTest(GC$Web.Area,NC$Web.Area)
TTest(LV$Web.Area,NC$Web.Area)



# Figures -----------------------------------------------------------------

#For each of your 4 web variables on which you performed t-tests you also need to provide a figure
#Here we will be making boxplots using the boxplot() function

boxplot(Data$Web.Height~Data$Spider.ID,xlab='Spider species',ylab='Web height (cm)',cex.axis=0.75)

#You should now make the rest of the boxplots.
#Make sure all boxplots have appropriate axis labels
#When you copy them into your report, remember to give them each a caption

boxplot(Data$Body.Length~Data$Spider.ID,xlab='Spider species',ylab='Body length (mm)',cex.axis=0.75)
boxplot(Data$Strand.density~Data$Spider.ID,xlab='Spider species',ylab='Strand density (N)',cex.axis=0.75)
boxplot(Data$Number.of.Radii~Data$Spider.ID,xlab='Spider species',ylab='Number of radii (N)',cex.axis=0.75)
boxplot(Data$Orientation~Data$Spider.ID,xlab='Spider species',ylab='Orientaiton (degrees)',cex.axis=0.75)
boxplot(Data$Web.Area~Data$Spider.ID,xlab='Spider species',ylab='Web Area (cm^2)',cex.axis=0.75)

# Chi-square test ---------------------------------------------------------

#Lastly we will perform a chi-square test
#Remember! A chi-square test compares frequencies between two categorical variables

#Here we will do a chi-square test on web placement

Observed<-table(Data$Spider.ID,Data$Substrate)#make a table of frequencies
Observed 

Test=chisq.test(Observed) #Now lets run the test
Test

#If we see that its significant, we next need to look at how it is significant
#To do this we compare the observed to expected tables
Test$observed #Here is our observed table
Test$expected #Here is our expected table
#How are these two different for each species?


#Remember! for a chi-square test always compare observed to expected tables, not within observed table



