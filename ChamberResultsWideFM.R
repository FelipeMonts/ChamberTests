#read data
#setwd('/Users/allis/OneDrive/Documents/N2O Project/ChamberPaper/GCResultsChamberTest')

setwd("C:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\AllisCode")
chamber.0 <- read.csv('ChamberResults.csv')
str(chamber.0)
head(chamber.0)

#Read new results 2021/11/02

chamber.new<-read.csv('ChamberResultsNew.csv')
str(chamber.new)

# Note that there are two tests in the 'ChamberResultsNew.csv' file. The  new results are the ones identified with Test 5 (ï..Test = 5). We need to select the records with test 5 and aded to the dataframe


# Select the records with test =5

chamber.new[chamber.new$ï..Test == 5,]

#### Add the data of test 5 to the data frame

### use rbind to attach the new data to the data frame

chamber<-rbind(chamber.0 , chamber.new[chamber.new$ï..Test == 5,])

#### Add the new results (data of test 5)  to the data frame

#check

str(chamber)
head(chamber)

#install package to organize data
#install.packages("tidyr")


#Using the package tidyr, Alli's or5iginal choice


library(tidyr)
#organize data with time as a variable
wide_chamber <- chamber %>% spread(Time, Concentration)
head(wide_chamber)
#rename columns
colnames(wide_chamber)
names(wide_chamber)[names(wide_chamber)== "0"] <- "T0"
names(wide_chamber)[names(wide_chamber)== "10"] <- "T1"
names(wide_chamber)[names(wide_chamber)== "20"] <- "T2"
names(wide_chamber)[names(wide_chamber)== "30"] <- "T3"

str(wide_chamber)
head(wide_chamber)

# Using R basic functions, this preferred because you can follow what happens, with tidyr is some times unpredictable

#the function to use is reshape

str(chamber)
head(chamber)

wide_chamber_2<-reshape(data=chamber, timevar= "Time" , idvar=c("ï..Test" , "Plot", "Chamber"), direction = "wide") ;


# Check if the data fram was transformed right

str(wide_chamber_2)
head(wide_chamber_2)




#create a function to do a linear model on the data and collect the slope coefficient

# First, lets try to see how the linear model works on one data set

# as an example apply the linear model to the data in the first row data set

# first data set 

wide_chamber[1,]

# apply the linear model to the data. For that we need the data in a data frame form: once column for time, and a second for concentration

# the column for time is easy to create

Time<-c(0,10,20,30)

# the column for concentration requires a little bit more work because it is a row not a column.
wide_chamber[1,c("T0", "T1" , "T2" ,"T3")]

# that is easy to do with the transpose function t(). Transpose is a term used i matrix algebra that convert rowns into columns but preserving the data  structure

t(wide_chamber[1,c("T0", "T1" , "T2" ,"T3")])

# Once we have that we can create the data frame with the two columns needed

data.linearmodel<-data.frame(Time,t(wide_chamber[1,c("T0", "T1" , "T2" ,"T3")]))

data.linearmodel

# add the name "Concentration" to the second column

names(data.linearmodel)<-c("Time", "Concentration")

data.linearmodel

# Now we have the data in the right form. Lets do the linear model on the data

lm(Concentration~Time,data.linearmodel)

# We can save and retrive the results in a variable like Linear.Model.Results

Linear.Model.Results<-lm(Concentration~Time,data.linearmodel)

# the results are stored in a precise form. In a list with all the parameters of a regression model and can be accessed by their name

str(Linear.Model.Results)

# for example to access the coefficients

Linear.Model.Results$coefficients

# or the residuals
Linear.Model.Results$residuals

# Now lets put it together for all the rows in the wide_chamber data  and add it to the data.
# For that we can us a for loop, that will do the same process for each line in the wide wide_chamber.

# for that we will create a variable "i" that will take the value of each row number in the wide_chamber data and will change to the value of the next row when the process for the row is finished.

#what are the row numbers in wide_chamber

# 1, 2, 3, etc until the last one.

# the last one can be found with dim()

dim(wide_chamber)  # 40 (rows)  7 (columns)

# to create the row numbers in in wide_chamber we can use a sequence 

seq(from=1, to=dim(wide_chamber)[1])

# We can store the row numbers in wide_chamber.rownumbers

wide_chamber.rownumbers<-seq(from=1, to=dim(wide_chamber)[1])


# we need to create an empty data frame to collect the coefficients. We will call it Linear.Model.coeficients

Linear.Model.coeficients<-data.frame(Intercept=numeric(), Slope=numeric())

str(Linear.Model.coeficients)

# now run the for loop

for(i in wide_chamber.rownumbers) {
  
  #create the data frame to do the linear model for each row "i"
  data.linearmodel<-data.frame(Time,t(wide_chamber[i,c("T0", "T1" , "T2" ,"T3")]))
  
  # add the name "Concentration" to the second column
  names(data.linearmodel)<-c("Time", "Concentration") 
  
  #apply the linear model to the data and store the results
  Linear.Model.Results<-lm(Concentration~Time,data.linearmodel)
  
  # collect the coefficients 
  
  Linear.Model.Results$coefficients
  
  # Store the coefficients in the apropriate line number 
  
  Linear.Model.coeficients[i,]<- Linear.Model.Results$coefficients
  
  
  
}

str( Linear.Model.coeficients)
head(Linear.Model.coeficients)


# Now we added to the wide_chamber data

wide_chamber.AND.Coeff<-data.frame(wide_chamber,Linear.Model.coeficients )

# check

str( wide_chamber.AND.Coeff)
head(wide_chamber.AND.Coeff)

# The variables Plot, Chamber are read as characters and the variable ï..Test  as an integer. 
# transforming those variables into factors will greatly facilitate the analysis.
# Lets create new factors based on these variables

#First ï..Test

wide_chamber.AND.Coeff$Test.Factor<-as.factor(wide_chamber.AND.Coeff$ï..Test)


#then Plot 

wide_chamber.AND.Coeff$Plot.Factor<-as.factor(wide_chamber.AND.Coeff$Plot)



#finally Chamber

wide_chamber.AND.Coeff$Chamber.Factor<-as.factor(wide_chamber.AND.Coeff$Chamber)

#check

str( wide_chamber.AND.Coeff)
head(wide_chamber.AND.Coeff)

