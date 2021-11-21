#read data
setwd('/Users/allis/OneDrive/Documents/N2O Project/ChamberPaper/GCResultsChamberTest')
chamber <- read.csv('ChamberResults.csv')

#install package to organize data
install.packages("tidyr")
library(tidyr)
#organize data with time as a variable
wide_chamber <- chamber %>% spread(Time, Concentration)
head(wide_chamber,40)
#rename columns
colnames(wide_chamber)
names(wide_chamber)[names(wide_chamber)== "0"] <- "T0"
names(wide_chamber)[names(wide_chamber)== "10"] <- "T1"
names(wide_chamber)[names(wide_chamber)== "20"] <- "T2"
names(wide_chamber)[names(wide_chamber)== "30"] <- "T3"
#add the slope as a column
spl <- with (Concentration, split(Concentration, list(Test = Test,Plot = Plot, Chamber = Chamber)))
coefLM <- function(x){
  coef(lm())
}


library(dplyr)
library(broom)
fit_model <- function(wide_chamber) lm(Value ~Year, data = Wide_chamber)
get_slope <- function(mod) tidy(mod)$estimate[2]
