#load data
# setwd('/Users/allis/OneDrive/Documents/N2O Project/ChamberPaper/GCResultsChamberTest')
# chamber <- read.csv('ChamberResults.csv')

setwd("C:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\AllisCode")



chamber.0 <- read.csv('ChamberResults.csv')

#in what form was the data read? str can tell us
str(chamber.0)

#Read new results 2021/11/02

chamber.new<-read.csv('ChamberResultsNew.csv')
str(chamber.new)


# Note that there are two tests in the 'ChamberResultsNew.csv' file. The  new results are the ones identified with Test 5 (ï..Test = 5). We need to select the records with test 5 and aded to the dataframe


# Select the records with test =5

chamber.new[chamber.new$ï..Test == 5,]

#### Add the data of test 5 to the data frame

### use rbind to attach the new data to the data frame

chamber<-rbind(chamber.0 , chamber.new[chamber.new$ï..Test == 5,])

# check

str(chamber)
head(chamber)


#in what form was the data read? str can tell us
str(chamber)


# 'data.frame':	239 obs. of  5 variables:
# $ ï..Test      : int  3 3 3 3 3 3 3 3 3 3 ...
# $ Plot         : chr  "Fallow-1" "Fallow-1" "Fallow-1" "Fallow-2" ...
# $ Chamber      : chr  "Manual" "Manual" "Manual" "Manual" ...
# $ Time         : int  0 10 20 0 10 20 30 0 10 20 ...
# $ Concentration: num  392 609 597 454 592 ...

# The variables Plot, Chamber are read as characters and the variable ï..Test  as an integer. 
# transforming those variables into factors will greatly facilitate the analysis.
# Lets create new factors based on these variables

#First ï..Test

chamber$Test.Factor<-as.factor(chamber$ï..Test)

chamber.new$Test.Factor<-as.factor(chamber.new$ï..Test)

chamber$Plot.Factor<-as.factor(chamber$Plot)

chamber.new$Plot.Factor<-as.factor(chamber.new$Plot)

#finally Chamber

chamber$Chamber.Factor<-as.factor(chamber$Chamber)

chamber.new$Chamber.Factor<-as.factor(chamber.new$Chamber)

# Select the columns for the splitting and  analysis, and collect them in the file chamber.1

str(chamber)

chamber.1<-chamber[,c("Test.Factor" , "Plot.Factor" ,   "Chamber.Factor", "Time","Concentration")]



str(chamber.1)
head(chamber.1)



###############################################################################################################
# Write the chamber data in to a file in order to create the graphics with the concentration of each chamber with time                          
###############################################################################################################


write.csv(chamber.1, file="ChamberConcentrationData.csv", row.names=F)



#need to still convert to correct units



#split the data into the relevant chunk of data
spl <- with (chamber.1, split(chamber.1, list(Test.Factor , Plot.Factor ,   Chamber.Factor)))

str(spl)

# spl is a list that contain 40 data frames each containing the data that was splitted for example the firts element on the list is spl[[1]]

spl[[1]]

str(spl[[1]])

# the 10th element is

spl[[10]]

str(spl[[10]])

# and the 40th element is 

spl[[40]]

str(spl[[40]])



# #function to fit a linear model and return the slope coefficient
# coefLM <- function(x){
#   coef(lm(Concentration ~ Time, data = x))
# }


#Lets change the function coefLM commented above from collecting only the slope coefficient, to collecting the slope and the intercept coefficients

# try lm first with the fist data set of spl, spl[1]
names(spl[[1]])

linear.model<-lm(Concentration~Time,spl[[1]]) ;

# get the variance covariance

vcov(linear.model)

# plot the data with the linear model

# plot the data
plot (spl[[1]]$Time, spl[[1]]$Concentration, col="RED", pch=16, cex=2, ylim=c(0,2000)) ;

# plot the linear model
abline(linear.model, col="BLUE",lwd=4);

# create the data to plot the confidence interval lines

# first create the x data for the confidence interval lines using a sequence from the minimum time in the spl[[1]]$Time data " min(spl[[1]]$Time)  " and the maximum time in the spl[[1]]$Time data "max(spl[[1]]$Time)" 

pred.x<-seq(min(spl[[1]]$Time), max(spl[[1]]$Time),by=1) ;

print(pred.x)

# calculate the confidence interval using the prediction function (predict) of the linear model we already calculated at the points we created pred.x. The function predict with the interval parameter set to "confidence" will output a prediction according to the linear model "fit" and a lower "lwr" and upper "upr" 95 % confidence interval

conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)

print(conf_interval)

# we add lines with the upper and lowre confidence intervals to the plot

lines(pred.x, conf_interval[,2], col="lightblue", lty=2, lwd=3)

lines(pred.x, conf_interval[,3], col="lightblue", lty=2, lwd=3)

#  Alli originally created a function to fit the linear model 

#function to fit a linear model and return the intercept and slope coefficients
coefLM.2 <- function(x){
  coef(lm(Concentration ~ Time, data = x))
}

# lets try the function with some data from spl.

# first data set from spl  spl[1]

spl[[1]]

coefLM.2(spl[[1]])



#apply function to each chunk of data using sapply 
coefs <- sapply(spl, coefLM.2)


head(coefs)


str(coefs)

# the results of str(coefs) indicate that it is a named matrix with two rows ("(Intercept)" "Time") and 40 columns
# num [1:2, 1:40] 363.9 12.5 417.2 23.2 477.9 ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr [1:2] "(Intercept)" "Time"
# ..$ : chr [1:40] "3.Fallow-1.Automatic" "4.Fallow-1.Automatic" "3.Fallow-10.Automatic" "4.Fallow-10.Automatic" ...


#now we want to create a new data frame with the coefficients as new columns added to the original information in chamber.1 
#for that remember that coefs is a named matrix wit two rows ("(Intercept)" "Time") and 40 columns
#  to make it easier to put together the data lets transform coefs into a matrix of two columns ("(Intercept)" "Time") and 40 rows using the transpose function t()

t(coefs)

# t(coefs) transformed coefs into a matrix with two colums and 40 rows, but the names of each row are not yet data. The names are only the names of the rows. We need to include a third column with the names of the rows as data
str(t(coefs))

#the names of the matrix t(coefs) can be accesed using the function dimnames()
dimnames(t(coefs))

# lets create a data frame with the data in t(coefs) and the names of t(coefs) obtained  with dimnanes ())


coefs.data.frame<-data.frame(dimnames(t(coefs))[[1]], t(coefs))

str(coefs.data.frame)

# the name of the columns of coefs.data.frame are not very decriptive, lets change them to something more useful
names(coefs.data.frame)<-c("Test.Plot.Chamber", "Intercept" , "Slope")

str(coefs.data.frame)

#much better

# now lets get the original data in chamber.1 from spl, making sure the data in coefs.data.frame matches the other data.
# for that we are going  to extract from each data frame in the spl list the first line, corresponding to time 0 . Time =0 
spl[[1]]

spl[[2]]

# we can do that with a for loop, or with sapply

# lets try with sapply

# First row and first column  [1,1] of each dataframe in spl
sapply(X=spl,FUN=function(x) x[1,1])


# First row and second column  [1,2] of each dataframe in spl
sapply(X=spl,FUN=function(x) x[1,2])

# First row and third column  [1,3] of each dataframe in spl
sapply(X=spl,FUN=function(x) x[1,3])

# First row and fourth column  [1,4] of each dataframe in spl
sapply(X=spl,FUN=function(x) x[1,4])

# First row and fifth column  [1,5] of each dataframe in spl
sapply(X=spl,FUN=function(x) x[1,5])

#Lets put it together in a data frame 

coefs.otherdata<-data.frame(sapply(X=spl,FUN=function(x) x[1,1]), sapply(X=spl,FUN=function(x) x[1,2]), sapply(X=spl,FUN=function(x) x[1,3]), sapply(X=spl,FUN=function(x) x[1,4]), sapply(X=spl,FUN=function(x) x[1,5])  )

str(coefs.otherdata)
head(coefs.otherdata)
# The names of the columns are not useful. Lets change the names to something informative

names(coefs.otherdata)<-c("Test.Factor" , "Plot.Factor" ,   "Chamber.Factor", "Time","Concentration")


str(coefs.otherdata)
head(coefs.otherdata)

#much better.

#we are going to match the data in coefs.otherdata with the correct data in coefs.data.frame. and we are going to use the  variable "Test.Plot.Chamber". We need to create the same variable in coefs.otherdata to be able to match it and be sure the data in both is correct

# create the variable "Test.Plot.Chamber" in the coefs.otherdata data frame from the names of the dimensions that are stored in coefs.otherdata

# get the dimensions names with dimnames

dimnames(coefs.otherdata)
str(dimnames(coefs.otherdata))


# dimnames is a list with two vectors: one has the row names dimnames(coefs.otherdata)[[1]], and the other has the column names dimnames(coefs.otherdata)[[2]]

#we wnat the row names 

coefs.otherdata$Test.Plot.Chamber<-dimnames(coefs.otherdata)[[1]]

str(coefs.otherdata)

head(coefs.otherdata)

# now lets combine the two data frames using merge()

str(coefs.data.frame)

merge(coefs.otherdata,coefs.data.frame, by=c("Test.Plot.Chamber") )

# store it in a new data frame called Data.AND.Coefficients

Data.AND.Coefficients<-merge(coefs.otherdata,coefs.data.frame, by=c("Test.Plot.Chamber") )


str(Data.AND.Coefficients)
head(Data.AND.Coefficients)

# at this point we have the data we need for the analysis. Lets store it in a file that we can refer to for the analysis.

write.csv(Data.AND.Coefficients, file= "Data.AND.Coefficients.csv", row.names=F)





