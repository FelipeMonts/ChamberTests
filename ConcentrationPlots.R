##############################################################################################################
# 
# 
# Program to plot the CO2 concentration evolution on each chamber and each test
#  
# 
#  Felipe Montes 2022/01/16
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


#install.packages("RColorBrewer" , "colorRamps" , dependencies = T)



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

#library(openxlsx)

#library(lattice)


###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################

# The data was produces using the R code ChamberResults2FM.R, and was saved with the name ChamberConcentrationData.csv

# Read the chamber concentration data into a Dataframe


Chamber.Conc.Data<-read.csv('ChamberConcentrationData.csv', header=T) ;

# check 

str(Chamber.Conc.Data)

head(Chamber.Conc.Data)

# Because we read the data the factors are only strings and not factors yet.

# Create the factors that are needed from the strings, and named with better names

Chamber.Conc.Data$Test.Factor<-as.factor(Chamber.Conc.Data$Test.Factor)

# change the names of the levels of the factor to better names

levels(Chamber.Conc.Data$Test.Factor)<-c("October 1", "October 12", "October 27") ;

Chamber.Conc.Data$Chamber.Factor<-as.factor(Chamber.Conc.Data$Chamber.Factor) ;

levels(Chamber.Conc.Data$Chamber.Factor)<-c("Semi-automatic", "Manual")


# check

str(Chamber.Conc.Data)

head(Chamber.Conc.Data)


# Simplify the names of the plot factor. instead of Fallow-1, Fallow-2, etc get just 1, 2, 3

# use strsplit to split the name "Fallow-1" into two strings using the "-" to separate them.

strsplit(as.character(Chamber.Conc.Data$Plot.Factor), split="-")

#The result is a list with two elements in each component of the list: the first is the fist part of the string we split "Fallow", and the second one is the secomd part of the srting we split "1"

# for example check the first components of the list

strsplit(as.character(Chamber.Conc.Data$Plot.Factor), split="-")[[1]]

# now the 220th component of the list

strsplit(as.character(Chamber.Conc.Data$Plot.Factor), split="-")[[220]]

# to ge the second part of the stip of each component we use sapply combined with the function extract [] and exttract only the second element of each component function(x) x[2])

sapply(strsplit(as.character(Chamber.Conc.Data$Plot.Factor), split="-"), FUN=function(x) x[2])

# The result is a vector of strings with the number of each component only. 

#change the vector of strings to a vector of integers based on the string; that is convert "2" to 2.

as.integer(sapply(strsplit(as.character(Chamber.Conc.Data$Plot.Factor), split="-"), FUN=function(x) x[2]))

# now convert the number to a factor so we can groups things by plot factor (frame) 

as.factor(as.integer(sapply(strsplit(as.character(Chamber.Conc.Data$Plot.Factor), split="-"), FUN=function(x) x[2])))

# finally add this colum to the data frame


Chamber.Conc.Data$Frame<-as.factor(as.integer(sapply(strsplit(as.character(Chamber.Conc.Data$Plot.Factor), split="-"), FUN=function(x) x[2])))

# check

str(Chamber.Conc.Data)
head(Chamber.Conc.Data)

### Call a nice package for graphs colors

library(RColorBrewer)
library(colorRamps)
library(lattice)

colors<-brewer.pal(10, "Set3") ;


#   Plot all together
xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Plot.Factor), data=Chamber.Conc.Data, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))


# plot only October 1, "Semi-automatic", frame 1

plot(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == "1", c("Time")],Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == "3", c("Concentration")], type="b", col="#00FF00")



#   Plot all individually

#October 1

 xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Manual", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



#October 12

xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Manual", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))

#October 27

xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Manual", ], cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))


# only plot October 27  and October 1


chamberOOct.1.27<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" | Chamber.Conc.Data$Test.Factor == "October 27",]

chamberOOct.1.12<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" | Chamber.Conc.Data$Test.Factor == "October 12",]

xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=chamberOOct.1.27, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col=gray(0.8, 1))))

################################################################################################################

#Create the graphs in postscript eps  or png format ready for publication 
#### Change the names of the test.factor , chamber factor and Plot. factor for the graphs

###############################################################################################################


png(file="AllData1.png", width = 10, height = 8, units="in", res=2400, pointsize = 1)


xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Frame), data=chamberOOct.1.27, cex=1.5, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1, cex=1.5),key=list(corner=c(0,0.88), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab= "Time (minutes)", ylab=expression('CO'[2]*' concentration (ppm)'), par.settings = list(strip.background=list(col=gray(0.8, 1)), par.ylab.text=list(cex=2), par.xlab.text=list(cex=2)), par.strip.text=list(col="BLACK", cex=1.5), ylim=c(200, 1600))


dev.off()

# only plot October 1  and October 12



png(file="AllData2.png", width = 10, height = 8, units="in", res=2400, pointsize = 1)


xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Frame), data=chamberOOct.1.12, cex=1.5, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1, cex=1.5),key=list(corner=c(0,0.88), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab= "Time (minutes)", ylab=expression('CO'[2]*' concentration (ppm)'), par.settings = list(strip.background=list(col=gray(0.8, 1)), par.ylab.text=list(cex=2), par.xlab.text=list(cex=2)), par.strip.text=list(col="BLACK", cex=1.5))


dev.off()

# only plot all the data

png(file="AllData3.png", width = 8, height = 11, units="in", res=2400, pointsize = 1)


xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Frame), data=Chamber.Conc.Data, cex=1.5, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1, cex=1.5),key=list(corner=c(0,0.88), columns=2, text=list(levels(Chamber.Conc.Data$Frame), cex=1), lines=list(col=primary.colors(10), type="b", pch=16)), xlab= "Time (minutes)", ylab=expression('CO'[2]*' concentration (ppm)'), par.settings = list(strip.background=list(col=gray(0.8, 1)), par.ylab.text=list(cex=2), par.xlab.text=list(cex=2)), par.strip.text=list(col="BLACK", cex=1.5))


dev.off()



#################################################################################

#### Exploring the potential defective points

str(Chamber.Conc.Data)

Questionable.Points<-rbind(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Manual" & (Chamber.Conc.Data$Frame == 3 | Chamber.Conc.Data$Frame == 4), ],
                           Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & (Chamber.Conc.Data$Frame == 4 | Chamber.Conc.Data$Frame == 10), ],
                           Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Manual" & (Chamber.Conc.Data$Frame == 7 | Chamber.Conc.Data$Frame == 10), ],
                           Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == 2, ],
                           Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Manual" & (Chamber.Conc.Data$Frame == 2 | Chamber.Conc.Data$Frame == 1), ])



xyplot(Concentration ~ Time |  Chamber.Factor + Test.Factor , groups = (Frame), data=Questionable.Points, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1,cex=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(Chamber.Conc.Data$Frame)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab="Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col=gray(0.8, 1))))

# The potential flaw points are not as bad as they seem.

#######################################################





#################################################################################################################
#
#
#    Try a different plot with regression lines
#
#
#
#################################################################################################################

## October 1st

str(Chamber.Conc.Data) ; head(Chamber.Conc.Data)

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

png(file="AllData3LinearR.png", width=3840, height=3840, pointsize=48)

# i = 1 

par( mar=c(5, 5, 4, 2) + 0.1)
par(mfrow=c(3,2))

### Semi automatic

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="", cex.axis=2, cex.lab=2, main="Semi-automatic, October 1 ", cex.main=2);

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
# i=1  
  data.lm<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, c("Time") ],Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  
  
  rm( linear.model,pred.x,conf_interval)
  
}

legend(x=0,y=1800,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1, cex=1.5,ncol=2 )



### Manual

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab="", xlab="", cex.axis=2, cex.lab=2, main="Manual, October 1 ", cex.main=2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, c("Time") ],Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 1" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  
  
  rm( linear.model,pred.x,conf_interval)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)



###################### October 12


### Semi automatic

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="", cex.axis=2, cex.lab=2, main="Semi-automatic, October 12 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, c("Time") ],Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  rm(linear.model,pred.x,conf_interval,data.lm)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


### Manual

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab="", xlab="", cex.axis=2, cex.lab=2, main="Manual, October 12 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, c("Time") ],Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 12" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  
  
  rm( linear.model,pred.x,conf_interval)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


###################### October 27


### Semi automatic

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)", cex.axis=2, cex.lab=2, main="Semi-automatic, October 27 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  
  data.lm<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, c("Time") ],Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Semi-automatic" & Chamber.Conc.Data$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  rm(linear.model,pred.x,conf_interval,data.lm)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


### Manual 

plot(NULL, xlim=c(0,30), ylim=c(0,1800), ylab="", xlab="Time (minutes)", cex.axis=2, cex.lab=2, main="Manual, October 27 ", cex.main=2)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = TRUE);

for (i in seq( 1,10,1)){   
  # i=1 
  data.lm<-Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, ];
  
  linear.model<-lm(Concentration~Time, data=data.lm) ;
  
  pred.x<-seq(min(data.lm$Time), max(data.lm$Time),by=1) ;
  
  conf_interval<-predict(linear.model, newdata = data.frame(Time=pred.x), interval="confidence", level = 0.95)
  
  
  
  #  plot(NULL, xlim=c(0,30), ylim=c(200,1000), ylab=expression('CO'[2]*' concentration (ppm)'), xlab="Time (minutes)")
  #  polygon(c(rev(pred.x), pred.x), c(rev(conf_interval[,3]),conf_interval[,2]),col=Colores.Banda[i], border = NA )
  
  points(Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, c("Time") ],Chamber.Conc.Data[Chamber.Conc.Data$Test.Factor == "October 27" & Chamber.Conc.Data$Chamber.Factor == "Manual" & Chamber.Conc.Data$Frame == i, c("Concentration")]  , col=Colores[i], pch=16, cex=2) ;
  abline(linear.model, col=Colores[i],lwd=3, lty=1);
  
  
  
  rm( linear.model,pred.x,conf_interval)
  
}

# legend(x=0,y=1000,legend=seq(from=1, to=10, by=1),col=Colores, pch=16, bty="n", lty=1)


dev.off()


####################################################################################################

# Try the HMR package for analysis 

install.packages("HMR", dep=T)

library(HMR)


# calculating the dispersion of the data for prefiltering based on the  T zero concentration

str(Chamber.Conc.Data) ; head(Chamber.Conc.Data)

T0.Concentrations<-Chamber.Conc.Data[Chamber.Conc.Data$Time == 0 ,]

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


Chamber.Conc.Data$Series<-paste0(Chamber.Conc.Data$Test.Factor, ".", Chamber.Conc.Data$Frame, ".", Chamber.Conc.Data$Chamber.Factor);

Chamber.Conc.Data$V<-17.34/1000 # m3

Chamber.Conc.Data$A<-0.17  # m2

Data.HMR<-Chamber.Conc.Data[,c("Series", "V" , "A" , "Time" , "Concentration")] ;

write.csv(Data.HMR, file= "Data.HMR.csv", row.names = F)

HMR("Data.HMR.csv", series = c("October 27.1.Manual", "October 27.1.Semi-automatic"), sep="," , LR.always = T)
