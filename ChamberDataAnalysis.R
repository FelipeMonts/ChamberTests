##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected for the semi automatics chamber paper   
#  
# 
#  Felipe Montes 2022/01/14
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################


#  Tell the program where the package libraries are  #####################

.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

setwd("C:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\AllisCode")



###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


#install.packages("" , dependencies = T)



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

#library(openxlsx)

#library(lattice)


###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################

 # The data on whihc the analysis is based was created in two different ways using the R scripts "ChamberResults2FM.R" and "ChamberResultsWideFM.R" and was saved under the names "Data.AND.Coefficients.csv" and "wide_chamber.AND.Coeff.csv" respectively. Both scripts produce the same data and the files should have the same data

#Lets import the files and check if the data is the same

Data.1<-read.csv(file="Data.AND.Coefficients.csv", header=T) ;
str(Data.1)
head(Data.1)


Data.2<-read.csv(file="wide_chamber.AND.Coeff.csv", header=T) ;
str(Data.2)
head(Data.2)


# lets compare the data

Data.1[,c("Test.Factor" , "Plot.Factor" , "Chamber.Factor" , "Intercept"  ,   "Slope")]

Data.2[,c("Test.Factor" , "Plot.Factor" , "Chamber.Factor" , "Intercept"  ,   "Slope")]

setdiff(Data.1[,c("Test.Factor") ] , Data.2[,c("Test.Factor")])

setequal(Data.1[,c("Test.Factor") ] , Data.2[,c("Test.Factor")])


setdiff(Data.1[,c("Plot.Factor") ] , Data.2[,c("Plot.Factor")])

setequal(Data.1[,c("Plot.Factor") ] , Data.2[,c("Plot.Factor")])


setdiff(Data.1[,c("Intercept") ] , Data.2[,c("Intercept")])

setequal(Data.1[,c("Intercept") ] , Data.2[,c("Intercept")])

setdiff(Data.1[,c("Slope") ] , Data.2[,c("Slope")])

setequal(Data.1[,c("Slope") ] , Data.2[,c("Slope")])


# another way to test if the data is the same

plot(Data.1[,c("Intercept") ], Data.2[,c("Intercept")])

plot(Data.1[,c("Slope") ], Data.2[,c("Slope")])

# another way using all.equal

all.equal(Data.1[,c("Test.Factor" , "Plot.Factor" , "Chamber.Factor" , "Intercept"  ,   "Slope")], Data.2[,c("Test.Factor" , "Plot.Factor" , "Chamber.Factor" , "Intercept"  ,   "Slope")])




#####################################################################################################################
# 
# 
#  We will proceed the analysis with the data in the file  "Data.AND.Coefficients.csv" 
# 
####################################################################################################################

# read the data into R

Data.AND.Coefficients<-read.csv(file="Data.AND.Coefficients.csv", header=T) ;

str(Data.AND.Coefficients)
head(Data.AND.Coefficients)

# Because we read the table anew we need to tell R which columns are treatments and not just characters or integers

Data.AND.Coefficients$Test.Factor<-as.factor(Data.AND.Coefficients$Test.Factor);

Data.AND.Coefficients$Plot.Factor<-as.factor(Data.AND.Coefficients$Plot.Factor);

Data.AND.Coefficients$Chamber.Factor<-as.factor(Data.AND.Coefficients$Chamber.Factor);

# check

str(Data.AND.Coefficients)



# Calculate emission rate in kg/ha/d

# slope ppm (1 vol/1000 vol) /min * Volume / Area 
# 
# PV=nRT  -> n=PV/RT  
# 
# volume (L) = 17.34161048
# 
# Surface Area (m2) = 0.170685143 
# 
# R L*atm/Mol*K = 0.08206
# 
# T=293
# P=0.964949
#

# CO2 = 44.01 g/MOL

Slope.Factor.Mol_m2_min = ((17.34161048/0.170685143) * 0.964949 / 1000000) / (0.08206 * 293)

Slope.Factor.gm_m2_min = Slope.Factor.Mol_m2_min * 44.01

Slope.Factor.kg_ha_d = (Slope.Factor.gm_m2_min * 10000 * 60 * 24) /1000 

str(Data.AND.Coefficients)

Data.AND.Coefficients$Mol_m2_min<-Data.AND.Coefficients$Slope * Slope.Factor.Mol_m2_min

Data.AND.Coefficients$gm_m2_min<-Data.AND.Coefficients$Slope * Slope.Factor.gm_m2_min

Data.AND.Coefficients$kg_ha_d<-Data.AND.Coefficients$Slope * Slope.Factor.kg_ha_d

### Lets try the anova with the data set

str(Data.AND.Coefficients)
head(Data.AND.Coefficients)

#One way ANOVA

AOV.ONE<-aov(Slope~Plot.Factor, data=Data.AND.Coefficients)

summary(AOV.ONE)

#full anova
AOV.FULL<-aov(kg_ha_d~Plot.Factor + Test.Factor * Chamber.Factor, data=Data.AND.Coefficients)

summary(AOV.FULL)



# Test Normality of the residuals

residuals(AOV.FULL)
fitted(AOV.FULL)
plot(AOV.FULL)

plot(fitted(AOV.FULL),residuals(AOV.FULL) )
abline(h=0, lty=2, col="RED")

plot(fitted(AOV.FULL), rstudent(AOV.FULL))
plot(fitted(AOV.FULL), rstandard (AOV.FULL))


qqnorm(residuals(AOV.FULL))
qqline(residuals(AOV.FULL))
shapiro.test(residuals(AOV.FULL))
hist(residuals(AOV.FULL))


# Test the log transformation
str(Data.AND.Coefficients)
head(Data.AND.Coefficients)

# add the log transformed data to the data frame

Data.AND.Coefficients$Log.kg_ha_d<-log(Data.AND.Coefficients$kg_ha_d) ;


#check

str(Data.AND.Coefficients)
head(Data.AND.Coefficients)

# do the same analysis as before

#full anova
AOV.FULL.Log<-aov(Log.kg_ha_d~Plot.Factor + Test.Factor * Chamber.Factor, data=Data.AND.Coefficients) ;

summary(AOV.FULL.Log)

# no change on the results


# Test Normality of the residuals

plot(AOV.FULL.Log)

plot(fitted(AOV.FULL.Log),residuals(AOV.FULL.Log) )
abline(h=0, lty=2, col="RED")

plot(fitted(AOV.FULL.Log), rstudent(AOV.FULL.Log))
plot(fitted(AOV.FULL.Log), rstandard (AOV.FULL.Log))


qqnorm(residuals(AOV.FULL.Log))
qqline(residuals(AOV.FULL.Log))
shapiro.test(residuals(AOV.FULL.Log))
hist(residuals(AOV.FULL.Log))

#####################################################################################################################
# 
# The residuals are normal in both the original data and in the log transformed. Ther is not need to do a transformation!
#  
# 
####################################################################################################################



  
  
  
#### Some more exploration.

# one thing that influences a lot the slope of the regression is the concentration at T0. Lets check it.

plot(Data.AND.Coefficients$Concentration)



# plot the t0 data by tests (dates)

plot(Data.AND.Coefficients[Data.AND.Coefficients$Test.Factor == 3, c("Concentration")], col="BLUE", ylim=c(min(Data.AND.Coefficients$Concentration), max(Data.AND.Coefficients$Concentration)))
                                                                                                           
points(Data.AND.Coefficients[Data.AND.Coefficients$Test.Factor == 4, c("Concentration")], col="RED")
points(Data.AND.Coefficients[Data.AND.Coefficients$Test.Factor == 5, c("Concentration")], col="Green")


# clearly there is an outlier in the concentration at T0 Lets find out who it is.

Data.AND.Coefficients[Data.AND.Coefficients$Concentration>=550,]


########## Plotting the data
str(Data.AND.Coefficients)

Data.AND.Coefficients$Date<-Data.AND.Coefficients$Test.Factor;

levels(Data.AND.Coefficients$Date)<-list("October 1" = "3", "October 12" = "4" ,"October 27" = "5" ) ;

str(Data.AND.Coefficients)


library(lattice)

### Box whiskers plot of the emission rates for automatic and Manual chamber, on each day

bwplot(kg_ha_d~Chamber.Factor| Date,  data=Data.AND.Coefficients, ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1))


### Box whiskers plot of the initial CO2 concentrations rates for automatic and Manual chamber, on each day

bwplot(Concentration~Chamber.Factor| Date, data=Data.AND.Coefficients,  ylab=expression('CO'[2]*~ 'concentration ppm'))


# There are two very large rates in the automatic chamber, higher than 40. Lets find out which ones are these

Data.AND.Coefficients[Data.AND.Coefficients$Slope>=40,]

# >  Data.AND.Coefficients[Data.AND.Coefficients$Slope>=40,]
# Test.Plot.Chamber Test.Factor Plot.Factor Chamber.Factor Time Concentration Intercept    Slope Log.Slope
# 33 4.Fallow-6.Automatic           4    Fallow-6      Automatic    0      368.1008  363.2122 43.33147  3.768879
# 37 4.Fallow-8.Automatic           4    Fallow-8      Automatic    0      308.4381  284.5666 40.63332  3.704588
# >

# These two very large rates have very low T0 concentration.

# Could T0 concentration being the culprit of those very high rates?

# Lets explore the T0 concentrations

xyplot(Concentration~Chamber.Factor, data=Data.AND.Coefficients)

bwplot(Concentration~Chamber.Factor, data=Data.AND.Coefficients)

## It does not appear that the initial concentration has anything to do with the high slopes



####################################################################################################

## Plotting  together with the Gasmet data

# Read the Gasmet  and the matching semiautomatic chamber data which was processed with the R code GasmetResults.R

Gasmet.Flux.1<-read.csv('GasmetFlux.csv', header=T) ;

#check

str(Gasmet.Flux.1) 
head(Gasmet.Flux.1) 


SemiAuto.Flux.1<-read.csv('SemiAutoFlux.csv', header=T) ;

#check

str(SemiAuto.Flux.1) 
head(SemiAuto.Flux.1) 

#Convert the data into the appropriate units

Gasmet.Flux.1$kg_ha_d<-as.numeric(Gasmet.Flux.1$Slope) * Slope.Factor.kg_ha_d ;

SemiAuto.Flux.1$kg_ha_d<-as.numeric(SemiAuto.Flux.1$Slope.CO2) * Slope.Factor.kg_ha_d ;


par( mar=c(5, 5, 4, 2) + 0.1)

png(file="AllData3_1to1.png", width=3840, height=3840, pointsize=48)

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



Data.AND.Coefficients$Date<-Data.AND.Coefficients$Test.Factor;

levels(Data.AND.Coefficients$Date)<-list("October 1" = "3", "October 12" = "4" ,"October 27" = "5" ) ;



####################################################################################################

## Plotting the raw emission data for alldata 

str(Data.AND.Coefficients)

str(Gasmet.Flux.1) 

str(SemiAuto.Flux.1)


#convert the Sample.Name in Gasmet.Flux.1 into a factor so we can use it to plot in groups

Gasmet.Flux.1$Plot.Factor<-as.factor(Gasmet.Flux.1$Sample.Name) ;


# plot all the data by chamber

xyplot(Slope~Chamber.Factor, data=Data.AND.Coefficients)

# box whiskers plot of all the data by chamber

bwplot(kg_ha_d~Chamber.Factor| Date,  data=Data.AND.Coefficients, ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1))

# box whiskers plot of all the data together 

boxplot(kg_ha_d ~ Chamber.Factor + Date,  data=Data.AND.Coefficients, ylab=expression('CO'[2]*~ 'emission rate'~ 'kg'^-1 ~ 'ha'^-1 ~ 'd'^-1))



########## Create a more sophisticated Box and whiskers plot of all the data together.

# for that we need to collect all the data to be plotted into a new data frame we will call BoxWiskers.data


BoxWiskers.data<-cbind(Data.AND.Coefficients[Data.AND.Coefficients$Date == "October 1" & Data.AND.Coefficients$Chamber.Factor == "Manual", c("kg_ha_d")], Data.AND.Coefficients[Data.AND.Coefficients$Date == "October 1" & Data.AND.Coefficients$Chamber.Factor == "Automatic", c("kg_ha_d")], Data.AND.Coefficients[Data.AND.Coefficients$Date == "October 12" & Data.AND.Coefficients$Chamber.Factor == "Manual", c("kg_ha_d")], Data.AND.Coefficients[Data.AND.Coefficients$Date == "October 12" & Data.AND.Coefficients$Chamber.Factor == "Automatic", c("kg_ha_d")], Data.AND.Coefficients[Data.AND.Coefficients$Date == "October 27" & Data.AND.Coefficients$Chamber.Factor == "Manual", c("kg_ha_d")], Data.AND.Coefficients[Data.AND.Coefficients$Date == "October 27" & Data.AND.Coefficients$Chamber.Factor == "Automatic", c("kg_ha_d")], Gasmet.Flux.1$kg_ha_d, SemiAuto.Flux.1$kg_ha_d) ;

str(BoxWiskers.data)

BoxWiskers.data.1<-as.data.frame(BoxWiskers.data);

str(BoxWiskers.data.1)

BoxWiskers.data.1[6:10, 7:8]<-NA



str(BoxWiskers.data.1)

names(BoxWiskers.data.1)<-c("Manual" ,  "Semi", "Manual" ,  "Semi", "Manual" ,  "Semi", "Gasmet" , "Semi") ;

png(file="AllData3_BW.png", width=3840, height=3840, pointsize=48) ;

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

