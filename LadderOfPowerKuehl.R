##############################################################################################################
# 
# 
# Program to  run the example the Ladder of Powers to find the best transformation a described in:
#Kuehl, R. O. 2000. Design of Experiments: Statistical Principles of Research Design and Analysis. 2nd ed. Pacific Grove, CA: Duxbury/Thomson Learning. Pages 135-139  
#  
# 
#  Felipe Montes 2022/01/15
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

setwd("C:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\AllisCode\\Kuehl_Ladder of Powers_P135_139")



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

# Read the data from example 4.1 in the book. Page 147, Apendix 4.A

Crab.data.1<-read.csv("Apendix4_1.txt", header=F, colClasses = "numeric") ;

str(Crab.data.1) ;

Crab.data<-data.matrix(t(Crab.data.1)) ;

summary(Crab.data)

Crab.data.mean<-apply(Crab.data, 2,mean)

Crab.data.mean.log<-log(Crab.data.mean)

Crab.data.sd<-apply(Crab.data, 2, sd)

Crab.data.sd.log<-log(Crab.data.sd)

Crab.data.log.mean.sd<-data.frame(Crab.data.mean.log,Crab.data.sd.log) ;

plot(Crab.data.log.mean.sd$Crab.data.mean.log,Crab.data.log.mean.sd$Crab.data.sd.log)

Crab.data.lm<-lm(Crab.data.log.mean.sd$Crab.data.sd.log~Crab.data.log.mean.sd$Crab.data.mean.log, data=Crab.data.log.mean.sd)

abline(Crab.data.lm, col="BLUE",lwd=4);

coefficients(Crab.data.lm)

# > coefficients(Crab.data.lm)
# (Intercept) Crab.data.log.mean.sd$Crab.data.mean.log 
# 0.6559222                                0.9857700 
# 

# Based on the data above, B=0.99, and p=1-0.99 =0.001 which is very close to 0. 
# the suggested transformation is logarithm, more specifically x=log(y + 1/6)

