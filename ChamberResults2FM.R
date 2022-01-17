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





 
 



# Try the HMR package for analysis 

install.packages("HMR", dep=T)

library(HMR)


# calculating the dispersion of the data for prefiltering based on the  T zero concentration

str(chamber.1) ; head(chamber.1)

T0.Concentrations<-chamber.1[chamber.1$Time == 0 ,]

## All
mean(T0.Concentrations[,c("Concentration") ]) #371.1288
median(T0.Concentrations[,c("Concentration") ]) # 391.4586
var(T0.Concentrations[,c("Concentration") ]) # 4548.059
sd(T0.Concentrations[, c("Concentration") ]) # 67.4393 



## October 1st

mean(T0.Concentrations[T0.Concentrations$Test.Factor == "October 1",c("Concentration") ]) # 292.485
median(T0.Concentrations[T0.Concentrations$Test.Factor == "October 1",c("Concentration") ]) # 289.8367
var(T0.Concentrations[T0.Concentrations$Test.Factor == "October 1",c("Concentration") ]) # 707.1513
sd(T0.Concentrations[T0.Concentrations$Test.Factor == "October 1",c("Concentration") ]) #26.59232



## October 12

mean(T0.Concentrations[T0.Concentrations$Test.Factor == "October 12",c("Concentration") ]) # 405.0755
median(T0.Concentrations[T0.Concentrations$Test.Factor == "October 12",c("Concentration") ]) # 401.6952
var(T0.Concentrations[T0.Concentrations$Test.Factor == "October 12",c("Concentration") ]) # 2656.669
sd(T0.Concentrations[T0.Concentrations$Test.Factor == "October 12",c("Concentration") ]) #51.54289


## October 27

mean(T0.Concentrations[T0.Concentrations$Test.Factor == "October 27",c("Concentration") ]) #  415.8261
median(T0.Concentrations[T0.Concentrations$Test.Factor == "October 27",c("Concentration") ]) # 413.107
var(T0.Concentrations[T0.Concentrations$Test.Factor == "October 27",c("Concentration") ]) # 932.7147
sd(T0.Concentrations[T0.Concentrations$Test.Factor == "October 27",c("Concentration") ]) #530.54038


# Preparing the data for the HMR Package. Creating the series name


chamber.1$Series<-paste0(chamber.1$Test.Factor, ".", chamber.1$Frame, ".", chamber.1$Chamber.Factor);

chamber.1$V<-17.34/1000 # m3
  
chamber.1$A<-0.17  # m2

Data.HMR<-chamber.1[,c("Series", "V" , "A" , "Time" , "Concentration")] ;

write.csv(Data.HMR, file= "Data.HMR.csv", row.names = F)

HMR("Data.HMR.csv", series = c("October 27.1.Manual", "October 27.1.Semi-automatic"), sep="," , LR.always = T)


#################################################################################################################
#
#
#    Try a different plot with regression bands
#
#
#
#################################################################################################################

## October 1st

str(chamber.1) ; head(chamber.1)

# plot the data with the linear model

# plot colors 
length(primary.colors())

#Colores<-primary.colors()[c(2,3,4,5,6,7,9,10,11,12,14,15,16,21,22,26)]

Colores<-primary.colors()[1:10]

Col.view<-c(seq(from=1, to=length(Colores), by=1))

barplot(Col.view, col=Colores,names.arg = Col.view )

Colores.Banda<-paste0(Colores, "22")



barplot(Col.view, col=Colores.Banda,names.arg = Col.view )



####################################################################################

png(file="AllData3.png", width=3840, height=3840, pointsize=48)

# i = 1 

par( mar=c(5, 5, 4, 2) + 0.1)
par(mfrow=c(3,2))

### Semi automatic

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="", cex.axis=2, cex.lab=2, main="Semi-automatic, October 1 ", cex.main=2);

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
 
  data.lm<-chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
#  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
#  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, c("Time") ],chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
   abline(linear.model, col=Colores[i],lwd=3, lty=1);
  

  
  rm( linear.model,pred.x,conf_interval)
  
}

legend(x=0,y=1800,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1, cex=1.5,ncol=2 )



### Manual

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab="", xlab="", cex.axis=2, cex.lab=2, main="Manual, October 1 ", cex.main=2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, c("Time") ],chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  
  
  rm( linear.model,pred.x,conf_interval)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)



###################### October 12


### Semi automatic

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="", cex.axis=2, cex.lab=2, main="Semi-automatic, October 12 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, c("Time") ],chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  rm(linear.model,pred.x,conf_interval,data.lm)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


### Manual

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab="", xlab="", cex.axis=2, cex.lab=2, main="Manual, October 12 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, c("Time") ],chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  
  
  rm( linear.model,pred.x,conf_interval)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


###################### October 27


### Semi automatic

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)", cex.axis=2, cex.lab=2, main="Semi-automatic, October 27 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, c("Time") ],chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  rm(linear.model,pred.x,conf_interval,data.lm)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


### Manual 

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab="", xlab="Time (minutes)", cex.axis=2, cex.lab=2, main="Manual, October 27 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
 # i=1 
  data.lm<-chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, c("Time") ],chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Manual" & chamber.1$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  
  
  rm( linear.model,pred.x,conf_interval)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


dev.off()



####################################################################################################

## Plotting  together with the Gasmet data

# Read the Gasmet data whcih was procesed with the R code GasmetResults.R

Gasmet.Flux.1<-read.csv('Gasmet.Flux.csv', header=T) :

str(Gasmet.Flux.1) 
head(Gasmet.Flux.1) 

#Convert the data into th eparopirate units

Gasmet.Flux.1$kg_ha_d<-as.numeric(Gasmet.Flux.1$Slope) * Slope.Factor.kg_ha_d

SemiAuto.Flux.1$kg_ha_d<-SemiAuto.Flux.1$Slope.CO2 * Slope.Factor.kg_ha_d

par( mar=c(5, 5, 4, 2) + 0.1)

png(file="AllData3_b.png", width=3840, height=3840, pointsize=48)

par( mar=c(8, 9, 4, 2) + 0.1)

par(mgp=c(6,2,0))

max(Data.AND.Coefficients$kg_ha_d) # 112

plot(Data.AND.Coefficients[Data.AND.Coefficients$Chamber.Factor == "Manual" & Data.AND.Coefficients$Test.Factor == 3, c("kg_ha_d")],Data.AND.Coefficients[Data.AND.Coefficients$Chamber.Factor == "Automatic"& Data.AND.Coefficients$Test.Factor == 3, c("kg_ha_d")], col="BLUE", pch=16, cex=3, xlab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1 ~'(Manual and Gasmet)'), ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1 ~'(Semi-automatic)'), cex.axis=3, cex.lab=3,xlim=c(0,120), ylim=c(0,120)) ;

points(Data.AND.Coefficients[Data.AND.Coefficients$Chamber.Factor == "Manual" & Data.AND.Coefficients$Test.Factor == 4, c("kg_ha_d")],Data.AND.Coefficients[Data.AND.Coefficients$Chamber.Factor == "Automatic"& Data.AND.Coefficients$Test.Factor == 4, c("kg_ha_d")], col="GREEN", pch=16, cex=3) ;

points(Data.AND.Coefficients[Data.AND.Coefficients$Chamber.Factor == "Manual" & Data.AND.Coefficients$Test.Factor == 5, c("kg_ha_d")],Data.AND.Coefficients[Data.AND.Coefficients$Chamber.Factor == "Automatic"& Data.AND.Coefficients$Test.Factor == 5, c("kg_ha_d")], col="PURPLE", pch=16, cex=3);

points(Gasmet.Flux.1$kg_ha_d,SemiAuto.Flux.1$kg_ha_d, col="RED", pch=16, cex=3)

abline(0, 1, col="BLACK", lwd=3)

legend(x=80,y=40,legend=c("Oct-1", "Oct-12", "Oct-27", "Gasmet"),col=c("BLUE", "GREEN" ,"PURPLE", "RED"  ), pch=16, bty="n", cex=3)

dev.off()

str(Data.AND.Coefficients.1)

Data.AND.Coefficients.1$Date<-Data.AND.Coefficients.1$Test.Factor;

levels(Data.AND.Coefficients.1$Date)<-list("October 1" = "3", "October 12" = "4" ,"October 27" = "5" ) ;



####################################################################################################

## Plotting the raw emission data for alldata 

str(Data.AND.Coefficients.1)

str(Gasmet.Flux.1) 

str(SemiAuto.Flux.1)

Gasmet.Flux.1$Plot.Factor<-as.factor(Gasmet.Flux.1$Sample.Name) ;




xyplot(Slope~Chamber.Factor, data=Data.AND.Coefficients.1)

bwplot(kg_ha_d~Chamber.Factor| Date,  data=Data.AND.Coefficients.1, ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1))

boxplot(kg_ha_d ~ Chamber.Factor + Date,  data=Data.AND.Coefficients.1, ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1))


BoxWiskers.data<-cbind(Data.AND.Coefficients.1[Data.AND.Coefficients.1$Date == "October 1" & Data.AND.Coefficients.1$Chamber.Factor == "Manual", c("kg_ha_d")], Data.AND.Coefficients.1[Data.AND.Coefficients.1$Date == "October 1" & Data.AND.Coefficients.1$Chamber.Factor == "Automatic", c("kg_ha_d")], Data.AND.Coefficients.1[Data.AND.Coefficients.1$Date == "October 12" & Data.AND.Coefficients.1$Chamber.Factor == "Manual", c("kg_ha_d")], Data.AND.Coefficients.1[Data.AND.Coefficients.1$Date == "October 12" & Data.AND.Coefficients.1$Chamber.Factor == "Automatic", c("kg_ha_d")], Data.AND.Coefficients.1[Data.AND.Coefficients.1$Date == "October 27" & Data.AND.Coefficients.1$Chamber.Factor == "Manual", c("kg_ha_d")], Data.AND.Coefficients.1[Data.AND.Coefficients.1$Date == "October 27" & Data.AND.Coefficients.1$Chamber.Factor == "Automatic", c("kg_ha_d")], Gasmet.Flux.1$kg_ha_d, SemiAuto.Flux.1$kg_ha_d) ;

str(BoxWiskers.data)

BoxWiskers.data.1<-as.data.frame(BoxWiskers.data);

BoxWiskers.data.1[6:10, 7:8]<-NA



str(BoxWiskers.data.1)

names(BoxWiskers.data.1)<-c("Manual" ,  "Semi", "Manual" ,  "Semi", "Manual" ,  "Semi", "Gasmet" , "Semi") ;

png(file="AllData3_c.png", width=3840, height=3840, pointsize=48) ;

par( mar=c(5, 8, 4, 2) + 0.1)

par(mgp=c(6,2,0))


boxplot(BoxWiskers.data.1, ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1), boxfill=c("BLUE","BLUE", "GREEN" ,"GREEN" , "PURPLE", "PURPLE", "RED" ,"RED"  ),cex.axis=2, cex.lab=2)

legend(x=0.5,y=120,legend=c("Oct-1", "Oct-12", "Oct-27", "Gasmet"),col=c("BLUE", "GREEN" ,"PURPLE", "RED"  ), pch=16, bty="n", cex=2)

dev.off()



####################################################################################################

## Plotting the differences in  emission data for all paired data

BoxWiskers.data.1$Oct.1<-BoxWiskers.data.1[,2] - BoxWiskers.data.1[,1] ;

BoxWiskers.data.1$Oct.12<-BoxWiskers.data.1[,4] - BoxWiskers.data.1[,3] ;

BoxWiskers.data.1$Oct.27<-BoxWiskers.data.1[,6] - BoxWiskers.data.1[,5] ;

BoxWiskers.data.1$GasmetD<-BoxWiskers.data.1[,8] - BoxWiskers.data.1[,7] ;

str(BoxWiskers.data.1)

#### Plot the differences

BoxWiskers.data.2<-BoxWiskers.data.1[, c("Oct.1" , "Oct.12", "Oct.27" , "GasmetD")]


png(file="Datadiff.png", width=3840, height=3840, pointsize=48) ;

par( mar=c(5, 8, 4, 2) + 0.1)

par(mgp=c(6,2,0))


boxplot(BoxWiskers.data.2, ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1), boxfill=c("BLUE", "GREEN" , "PURPLE",  "RED"  ),cex.axis=2, cex.lab=2)
abline(h=0, lty=3)

dev.off()

