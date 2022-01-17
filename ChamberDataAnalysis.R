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

# >  Data.AND.Coefficients.1[Data.AND.Coefficients.1$Slope>=40,]
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


#### plot of figure ### in the manuscript with all the data


### Call a nice package for graphs colors

library(RColorBrewer)
library(colorRamps)

colors<-brewer.pal(10, "Set3") ;

-#Create the graphs in postscript eps  or png format ready for publication 
# ?print.trellis 

#### Change the names of the test.factor , chamber factor and Plot. factor for the graphs


str(chamber.1) ; head(chamber.1)


levels(chamber.1$Test.Factor)<-c("October 1", "October 12", "October 27") ;

levels(chamber.1$Test.Factor)<-rev(levels(chamber.1$Test.Factor))

levels(chamber.1$Chamber.Factor)<-c("Semi-automatic" , "Manual") ;



sapply(strsplit(as.character(chamber.1$Plot.Factor), split="-"), FUN=function(x) x[2])

chamber.1$Frame<-as.factor(as.integer(sapply(strsplit(as.character(chamber.1$Plot.Factor), split="-"), FUN=function(x) x[2])))


#   Plot all together
xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))


#   Plot all individually

#October 1

xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Semi-automatic", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Manual", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



#October 12

xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Semi-automatic", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Manual", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))

#October 27

xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Semi-automatic", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Manual", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))


# only plot October 27  and October 1
str(chamber.1)

chamberOOct.1.27<-chamber.1[chamber.1$Test.Factor == "October 1" | chamber.1$Test.Factor == "October 27",]

chamberOOct.1.12<-chamber.1[chamber.1$Test.Factor == "October 1" | chamber.1$Test.Factor == "October 12",]

xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=chamberOOct.1.27, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col=gray(0.8, 1))))

############################################################


png(file="AllData1.png", width = 10, height = 8, units="in", res=2400, pointsize = 1)


xyplot(Concentration ~ Time | Chamber.Factor + Date.Factor, groups = (Frame), data=chamberOOct.1.27, cex=1.5, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1, cex=1.5),key=list(corner=c(0,0.88), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab= "Time (minutes)", ylab=expression('CO'[2]*' concentration (ppm)'), par.settings = list(strip.background=list(col=gray(0.8, 1)), par.ylab.text=list(cex=2), par.xlab.text=list(cex=2)), par.strip.text=list(col="BLACK", cex=1.5), ylim=c(200, 1600))


dev.off()

# only plot October 1  and October 12



png(file="AllData2.png", width = 10, height = 8, units="in", res=2400, pointsize = 1)


xyplot(Concentration ~ Time | Chamber.Factor + Date.Factor, groups = (Frame), data=chamberOOct.1.12, cex=1.5, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1, cex=1.5),key=list(corner=c(0,0.88), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab= "Time (minutes)", ylab=expression('CO'[2]*' concentration (ppm)'), par.settings = list(strip.background=list(col=gray(0.8, 1)), par.ylab.text=list(cex=2), par.xlab.text=list(cex=2)), par.strip.text=list(col="BLACK", cex=1.5))


dev.off()

# only plot all the data

png(file="AllData3.png", width = 8, height = 11, units="in", res=2400, pointsize = 1)


xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Frame), data=chamber.1, cex=1.5, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1, cex=1.5),key=list(corner=c(0,0.88), columns=2, text=list(levels(chamber.1$Frame), cex=1), lines=list(col=primary.colors(10), type="b", pch=16)), xlab= "Time (minutes)", ylab=expression('CO'[2]*' concentration (ppm)'), par.settings = list(strip.background=list(col=gray(0.8, 1)), par.ylab.text=list(cex=2), par.xlab.text=list(cex=2)), par.strip.text=list(col="BLACK", cex=1.5))


dev.off()



#################################################################################

#### Exploring the potential defective points

str(chamber.1)

Questionable.Points<-rbind(chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Manual" & (chamber.1$Frame == 3 | chamber.1$Frame == 4), ],
                           chamber.1[chamber.1$Test.Factor == "October 1" & chamber.1$Chamber.Factor == "Semi-automatic" & (chamber.1$Frame == 4 | chamber.1$Frame == 10), ],
                           chamber.1[chamber.1$Test.Factor == "October 12" & chamber.1$Chamber.Factor == "Manual" & (chamber.1$Frame == 7 | chamber.1$Frame == 10), ],
                           chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Semi-automatic" & chamber.1$Frame == 2, ],
                           chamber.1[chamber.1$Test.Factor == "October 27" & chamber.1$Chamber.Factor == "Manual" & (chamber.1$Frame == 2 | chamber.1$Frame == 1), ])



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Questionable.Points, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col=gray(0.8, 1))))

# The potential flaw points are not as bad as they seem.

#######################################################



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

