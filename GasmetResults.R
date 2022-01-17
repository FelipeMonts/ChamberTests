##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected Using the GASMET and the chamber that Curtis Dell at at USDA ARS uses   
#  and that it is also been used by Silesh in Heather Karsten's group
# 
#  Felipe Montes 2021/10/20
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

# readClipboard() Willow Rock Spring\\SkyCap_SelectionTrial\\DataCollection") ;   # 

"https://pennstateoffice365.sharepoint.com/:f:/s/StrategicTillageAndN2O/Ehl9Lh_gza5FiOtKIyDD7MQBOKFdFk6h_k4EEYEktWJUYw?e=uYLqL0"

# setwd("C:\\Users\\frm10\\Downloads\\SummaryReport")



###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


#install.packages("tabulizer", "tabulizerjars" , dependencies = T)



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)


###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored

Data.Home.Directory<-"C:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasmentChamberComparison"

Directory.List<-list.files(Data.Home.Directory);


Directory.List.1<-Directory.List[seq(3,16)] ;


##### Read one results file to see the structure

Test.1<-read.table(file=paste0(Data.Home.Directory, "\\" , Directory.List.1[8] , "\\Results\\Results.txt"), header=T, sep="\t") ;

str(Test.1)

Test.1$Date.Time<-as.POSIXct(Test.1$Date) ;

Test.2<-Test.1[,c("Date.Time" , "Carbon.dioxide.CO2..ppm.")] ;

Test.2$Time.Min<-as.numeric((Test.2$Date.Time - Test.2$Date.Time[1])/60) ;


###############################################################################################################
#                           Read all the files in the directory
###############################################################################################################

#### Read all the results with a for loop

## initialize the dataframe to collect all the data together

Gasmet.Data<-data.frame(Sample.Name = character(), Time =character(), CO2.ppm = numeric() ) ;   

for(i in Directory.List.1) {
  
  #i=Directory.List.1[1]
  
  DATA.1<-read.table(file=paste0(Data.Home.Directory, "\\" , i , "\\Results\\Results.txt"), header=T, sep="\t") ;
  
  DATA.1$Date.Time<-as.POSIXct(DATA.1$Date) ;
  
  DATA.2<-DATA.1[,c("Date.Time" , "Carbon.dioxide.CO2..ppm.")] ;
  
  DATA.2$Sample.Name<-i ;
  
  Gasmet.Data.0<-rbind(Gasmet.Data,DATA.2 );
  
  Gasmet.Data<-Gasmet.Data.0 ;
  
}

# convert sample name into a factor

Gasmet.Data$Sample.Factor<-as.factor(Gasmet.Data$Sample.Name) ;

head(Gasmet.Data)

# rename the colum with the CO2 concentration

names(Gasmet.Data)[2]<-"CO2ppm"


###############################################################################################################
#                          Explore the data
###############################################################################################################

# with water vapor filter and without water vapor filter

unique(Gasmet.Data$Sample.Name)

Gasmet.Data[seq(230,240), ] ;

Filter.test<-Gasmet.Data[seq(235,358), ] ;

Filter.test$Sample.Factor<-as.factor(Filter.test$Sample.Name) ;

xyplot(CO2ppm~Date.Time, data=Filter.test)


#### The first line of data does not start at T0, Need to correct that

# find the max which is the last data in each set


###############################################################################################################
#                          Arrange the data for flux calculations
###############################################################################################################
levels(Gasmet.Data$Sample.Factor)

for (j in levels(Gasmet.Data$Sample.Factor) ) {
  #j=levels(Gasmet.Data$Sample.Factor)[1]
  
  plot(Gasmet.Data[Gasmet.Data$Sample.Factor == j, c("Date.Time", "CO2ppm" )], main=j);
  text(Gasmet.Data[Gasmet.Data$Sample.Factor == j, c("Date.Time", "CO2ppm" )], labels=which(Gasmet.Data$Sample.Factor == j));
  
}

# 
# 
# p1 seq(4,14)
# 
# p2 seq(43,55)
# 
# p3 seq(65,88)
#
# p4 seq(95,114)
# 
# p5 c(117,seq(120,131))
#
# p6 seq(136,163)
# 
# p7 seq(166, 177) 
# 
# p8 c(184,seq(186, 197)) 
# 
# t1f c(240, seq(242,261))
# 
# tnf seq(272,291)
# 
# t2f seq(303,324)
# 
# t2nf seq(336,352)
# 
# 




###############################################################################################################
#                           Do the linear flux calculation
###############################################################################################################

# initiate data frame to collect all the flux data

Gasmet.Flux<-data.frame(Sample.Name = character(), T0.CO2ppm = numeric(), Max.CO2ppm=numeric(), Intercept = numeric(), Slope= numeric() )

str(Gasmet.Flux)

head(Gasmet.Data)

###### p1

# p1 seq(4,14)


p1.1<-4;
p1.f<-14;
p1.data<-Gasmet.Data[seq(p1.1,p1.f),] ;
p1.T0.time<-Gasmet.Data$Date.Time[p1.1] ;
p1.T0.CO2<-Gasmet.Data$CO2ppm[p1.1] ;
p1.Max.CO2<-Gasmet.Data$CO2ppm[p1.f] ;

p1.data$Time.min<-as.numeric((p1.data$Date.Time - p1.T0.time)/60) ;

p1.lm<-lm(CO2ppm~Time.min ,data= p1.data) ;

coefficients(p1.lm);


plot (p1.data$Time.min, p1.data$CO2ppm , main="p.1")
abline(a=coefficients(p1.lm)[1], b=coefficients(p1.lm)[2], col="RED")

Gasmet.Flux[1,]<- c("p1", p1.T0.CO2, p1.Max.CO2, coefficients(p1.lm) );


###### p2

# p2 seq(43,55)


p2.1<-43;
p2.f<-55;
p2.data<-Gasmet.Data[seq(p2.1,p2.f),] ;
p2.T0.time<-Gasmet.Data$Date.Time[p2.1] ;
p2.T0.CO2<-Gasmet.Data$CO2ppm[p2.1] ;
p2.Max.CO2<-Gasmet.Data$CO2ppm[p2.f] ;

p2.data$Time.min<-as.numeric((p2.data$Date.Time - p2.T0.time)/60)

p2.lm<-lm(CO2ppm~Time.min ,data= p2.data)

coefficients(p2.lm)


plot (p2.data$Time.min, p2.data$CO2ppm , main="p.2")
abline(a=coefficients(p2.lm)[1], b=coefficients(p2.lm)[2], col="RED")

Gasmet.Flux[2,]<- c("p2",p2.T0.CO2,p2.Max.CO2,coefficients(p2.lm) );



###### p3

# p3 seq(65,88)


p3.1<-65;
p3.f<-88;
p3.data<-Gasmet.Data[seq(p3.1,p3.f),] ;
p3.T0.time<-Gasmet.Data$Date.Time[p2.1] ;
p3.T0.CO2<-Gasmet.Data$CO2ppm[p3.1] ;
p3.Max.CO2<-Gasmet.Data$CO2ppm[p3.f] ;

p3.data$Time.min<-as.numeric((p3.data$Date.Time - p3.T0.time)/60)

p3.lm<-lm(CO2ppm~Time.min ,data= p3.data)

coefficients(p3.lm)


plot (p3.data$Time.min, p3.data$CO2ppm , main="p.3")
abline(a=coefficients(p3.lm)[1], b=coefficients(p3.lm)[2], col="RED")

Gasmet.Flux[3,]<- c("p3", p3.T0.CO2, p3.Max.CO2, coefficients(p3.lm) );


###### p4

# p4 seq(95,114)

p4.1<-95;
p4.f<-114;
p4.data<-Gasmet.Data[seq(p4.1,p4.f),] ;
p4.T0.time<-Gasmet.Data$Date.Time[p4.1] ;
p4.T0.CO2<-Gasmet.Data$CO2ppm[p4.1] ;
p4.Max.CO2<-Gasmet.Data$CO2ppm[p4.f] ;

p4.data$Time.min<-as.numeric((p4.data$Date.Time - p4.T0.time)/60) ;

p4.lm<-lm(CO2ppm~Time.min ,data= p4.data) ;

coefficients(p4.lm);


plot (p4.data$Time.min, p4.data$CO2ppm , main="p.4")
abline(a=coefficients(p4.lm)[1], b=coefficients(p4.lm)[2], col="RED")

Gasmet.Flux[4,]<- c("p4", p4.T0.CO2, p4.Max.CO2, coefficients(p4.lm) );



###### p5

# p5 c(117,seq(120,131))

p5.1<-117;
p5.f<-131;
p5.data<-Gasmet.Data[c(117,seq(120,131)),] ;
p5.T0.time<-Gasmet.Data$Date.Time[p5.1] ;
p5.T0.CO2<-Gasmet.Data$CO2ppm[p5.1] ;
p5.Max.CO2<-Gasmet.Data$CO2ppm[p5.f] ;

p5.data$Time.min<-as.numeric((p5.data$Date.Time - p5.T0.time)/60) ;

p5.lm<-lm(CO2ppm~Time.min ,data= p5.data) ;

coefficients(p5.lm);


plot (p5.data$Time.min, p5.data$CO2ppm , main="p.5")
abline(a=coefficients(p5.lm)[1], b=coefficients(p5.lm)[2], col="RED")

Gasmet.Flux[5,]<- c("p5", p5.T0.CO2, p5.Max.CO2, coefficients(p5.lm) );


###### p6

# p6 seq(136,163)


p6.1<-136;
p6.f<-163;
p6.data<-Gasmet.Data[seq(136,163),] ;
p6.T0.time<-Gasmet.Data$Date.Time[p6.1] ;
p6.T0.CO2<-Gasmet.Data$CO2ppm[p6.1] ;
p6.Max.CO2<-Gasmet.Data$CO2ppm[p6.f] ;

p6.data$Time.min<-as.numeric((p6.data$Date.Time - p6.T0.time)/60) ;

p6.lm<-lm(CO2ppm~Time.min ,data= p6.data) ;

coefficients(p6.lm);


plot (p6.data$Time.min, p6.data$CO2ppm , main="p.6")
abline(a=coefficients(p6.lm)[1], b=coefficients(p6.lm)[2], col="RED")

Gasmet.Flux[6,]<- c("p6", p6.T0.CO2, p6.Max.CO2, coefficients(p6.lm) );

###### p7

# p7 seq(166, 177) 

p7.1<-166;
p7.f<-177;
p7.data<-Gasmet.Data[seq(166, 177),] ;
p7.T0.time<-Gasmet.Data$Date.Time[p7.1] ;
p7.T0.CO2<-Gasmet.Data$CO2ppm[p7.1] ;
p7.Max.CO2<-Gasmet.Data$CO2ppm[p7.f] ;

p7.data$Time.min<-as.numeric((p7.data$Date.Time - p7.T0.time)/60) ;

p7.lm<-lm(CO2ppm~Time.min ,data= p7.data) ;

coefficients(p7.lm);


plot (p7.data$Time.min, p7.data$CO2ppm , main="p.7")
abline(a=coefficients(p7.lm)[1], b=coefficients(p7.lm)[2], col="RED")

Gasmet.Flux[7,]<- c("p7", p7.T0.CO2, p7.Max.CO2, coefficients(p7.lm) );


###### p8

# p8 c(184,seq(186, 197)) 

p8.1<-184;
p8.f<-197;
p8.data<-Gasmet.Data[c(184,seq(186, 197)),] ;
p8.T0.time<-Gasmet.Data$Date.Time[p8.1] ;
p8.T0.CO2<-Gasmet.Data$CO2ppm[p8.1] ;
p8.Max.CO2<-Gasmet.Data$CO2ppm[p8.f] ;

p8.data$Time.min<-as.numeric((p8.data$Date.Time - p8.T0.time)/60) ;

p8.lm<-lm(CO2ppm~Time.min ,data= p8.data) ;

coefficients(p8.lm);


plot (p8.data$Time.min, p8.data$CO2ppm , main="p.8")
abline(a=coefficients(p8.lm)[1], b=coefficients(p8.lm)[2], col="RED")

Gasmet.Flux[8,]<- c("p8", p8.T0.CO2, p8.Max.CO2, coefficients(p8.lm) );


###### p9


# p.9 seq(205, 234)

p9.1<-205;
p9.f<-234;
p9.data<-Gasmet.Data[seq(205, 234),] ;
p9.T0.time<-Gasmet.Data$Date.Time[p9.1] ;
p9.T0.CO2<-Gasmet.Data$CO2ppm[p9.1] ;
p9.Max.CO2<-Gasmet.Data$CO2ppm[p9.f] ;

p9.data$Time.min<-as.numeric((p9.data$Date.Time - p9.T0.time)/60) ;

p9.lm<-lm(CO2ppm~Time.min ,data= p9.data) ;

coefficients(p9.lm);


plot (p9.data$Time.min, p9.data$CO2ppm , main="p.9")
abline(a=coefficients(p9.lm)[1], b=coefficients(p9.lm)[2], col="RED")

Gasmet.Flux[9,]<- c("p9", p9.T0.CO2, p9.Max.CO2, coefficients(p9.lm) );


###### p10

# p.10 seq(20, 40)

p10.1<-20;
p10.f<-40;
p10.data<-Gasmet.Data[seq(20,40),] ;
p10.T0.time<-Gasmet.Data$Date.Time[p10.1] ;
p10.T0.CO2<-Gasmet.Data$CO2ppm[p10.1] ;
p10.Max.CO2<-Gasmet.Data$CO2ppm[p10.f] ;

p10.data$Time.min<-as.numeric((p10.data$Date.Time - p10.T0.time)/60) ;

p10.lm<-lm(CO2ppm~Time.min ,data= p10.data) ;

coefficients(p10.lm);


plot (p10.data$Time.min, p10.data$CO2ppm , main="p.10")
abline(a=coefficients(p10.lm)[1], b=coefficients(p10.lm)[2], col="RED")

Gasmet.Flux[10,]<- c("p10", p10.T0.CO2, p10.Max.CO2, coefficients(p10.lm) );

sapply(strsplit(Gasmet.Flux$Sample.Name, split= "p"), function(x) x[2])

Gasmet.Flux$Frame<-sapply(strsplit(Gasmet.Flux$Sample.Name, split= "p"), function(x) x[2]);


######## Compare the data with the Semi Automatic chambers


SemiAuto.Flux<-read.xlsx(xlsxFile=paste0(Data.Home.Directory,"\\","ChamberTestCalculationsREVISED20211025_FM.xlsx"), sheet = "Auto (2)")

sapply(strsplit(SemiAuto.Flux$Vial.Name, split="-"),function(x) x[2])


SemiAuto.Flux$Frame<-as.numeric(sapply(strsplit(SemiAuto.Flux$Vial.Name, split="-"),function(x) x[2]));



####### plot the data together

# str(Gasmet.Flux) ;  head(Gasmet.Flux)

plot(Gasmet.Flux$Frame,Gasmet.Flux$Slope, col="BLUE")
points(SemiAuto.Flux$Frame, SemiAuto.Flux$Slope.CO2, col="RED")

##### get the useful data sets out

Good.Frames<-c(1,2,5,7,8)


Gasmet.Flux.1<-Gasmet.Flux[Gasmet.Flux$Frame %in% Good.Frames, ]
SemiAuto.Flux.1<-SemiAuto.Flux[SemiAuto.Flux$Frame %in% Good.Frames, ]


plot(Gasmet.Flux.1$Frame,Gasmet.Flux.1$Slope, col="BLUE")
points(SemiAuto.Flux.1$Frame, SemiAuto.Flux.1$Slope.CO2, col="RED")


plot(Gasmet.Flux.1$Slope,SemiAuto.Flux.1$Slope.CO2, col="BLUE", xlim=c(20,50), ylim=c(20,50), cex=3, ylab=expression('CO'[2]*' concentration (ppm)-Semi-automatic'), xlab=expression('CO'[2]*' concentration (ppm)-Gasmet'),cex.axis=2, cex.lab=2)
abline(0, 1, col="RED", lwd=2)



######## write the useful Gasmet data "Gasmet.Flux.1" , and the matching Semiautomatic chamber fluxes to plot with the rest of the data if necessary
str(Gasmet.Flux.1)
head(Gasmet.Flux.1)

write.csv(Gasmet.Flux.1, file="C:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\AllisCode\\GasmetFlux.csv", row.names = F)

write.csv(SemiAuto.Flux.1, file="C:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\AllisCode\\SemiAutoFlux.csv", row.names = F)
