#load data
setwd('/Users/allis/OneDrive/Documents/N2O Project/ChamberPaper/GCResultsChamberTest')
chamber <- read.csv('ChamberResults.csv')
#need to still convert to correct units

#split the data into the relevant chunk of data
spl <- with (chamber, split(chamber, list(ï..Test = ï..Test,Plot = Plot,Chamber = Chamber)))

#function to fit a linear model and return the slope coefficient
coefLM <- function(x){
  coef(lm(Concentration ~ Time, data = x))[2]
}

#apply function to each chunk of data
coefs <- sapply(spl, coefLM)
head(coefs)

#create new chart with slope as a column
chamber_slope <- unique(chamber[, c("Plot","Chamber","ï..Test")])
chamber_slope <- transform(chamber_slope, flux = sapply(spl,coefLM))
chamber_slope

#function to fit a quadratic model
coefQM <- function(x){
  coef(lm(Concentration ~ Time + Time^2, data = x))[2]
}
coefq <- sapply(spl, coefQM)

#create new chart with flux as a column
chamber_flux <- unique(chamber[, c("Plot","Chamber","ï..Test")])
chamber_flux <- transform(chamber_flux, lm_flux = sapply(spl,coefLM))
chamber_flux <- transform(chamber_flux, qm_flux = sapply (spl,coefQM))

#ANOVA
one.way <- aov(flux ~ Plot, data = chamber_slope)
summary(one.way)

#Tukeys HSD
TukeyHSD(one.way, "flux", ordered = FALSE, conf.level=.95)
plot(Tukey, las=1)

#Test Assumption
#normally distributed
attach(chamber_slope)
qqnorm(flux)
qqline(flux)
shapiro.test(flux)
hist(flux)
#homogeneity of variance

