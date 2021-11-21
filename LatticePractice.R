install.packages("mlmRev", "MEMSS")
library("lattice")

data(Chem97, package = "mlmRev")
histogram(~ gcsescore | factor(score), data = Chem97)

densityplot(~ gcsescore | factor(score), data = Chem97,
            plot.points = FALSE, ref = TRUE)
densityplot(~ gcsescore | factor(score), data = Chem97,
            plot.points = T, ref = TRUE)
densityplot(~ gcsescore, data = Chem97, groups = score,
            plot.points = FALSE, ref = TRUE,
            auto.key = list(columns = 3))

data(Oats, package = "MEMSS")

str(chamber.1)

xyplot(Concentration ~ Time | Test.Factor  + Chamber.Factor , groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type="b", col=primary.colors(10) ,pch=16, auto.key = T)

xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type="b", col=primary.colors(10) ,pch=16, auto.key = T)

xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type="b", col=primary.colors(10) ,pch=16, auto.key = T, between = list(x = 0.5, y = 0.5))

xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type="b", col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5)
       
xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5))

xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same"))

xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1))


xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Plot.Factor)),points=list(pch=16, col=primary.colors(10), cex=0.8), lines=list(col=primary.colors(10), type="b", pch=16)))

xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type=c("b", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5), scales=list(x="same", y="same", alternating=1),key=list(corner=c(0,0.9), columns=1, text=list(levels(chamber.1$Plot.Factor)), lines=list(col=primary.colors(10), type="b", pch=16)), xlab= "Time (minutes)", ylab="CO2 concentration (ppm)", par.settings = list(strip.background=list(col="WHITE")))

Plot_a<-xyplot(Concentration ~ Time | Chamber.Factor + Test.Factor, groups = (Plot.Factor), data=chamber.1, cex=0.75, lwd=3, type=c("o", "g"), col=primary.colors(10) ,pch=16, draw.key = T, between = list(x = 0.5, y = 0.5))

str(Plot_a)

    

barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic),
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2))

barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic),
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2),
         scales = list(x = "free"))

update(bc.titanic,
       panel = function(..., border) {
         panel.barchart(..., border = "transparent")
       })

barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic),
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2),
         scales = list(x = "free"))

bc.titanic <-
  barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic),
           groups = Survived, stack = TRUE, layout = c(4, 1),
           auto.key = list(title = "Survived", columns = 2),
           scales = list(x = "free"))

update(bc.titanic, panel = panel.barchart)


update(bc.titanic,
       panel = function(...) {
         panel.grid(h = 0, v = -1)
         panel.barchart(...)
       })

xyplot(lat ~ long | cut(depth, 2), data = quakes)

xyplot(lat ~ long | cut(depth, 3), data = quakes,
       aspect = "iso", pch = ".", cex = 2, type = c("p", "g"),
       xlab = "Longitude", ylab = "Latitude",
       strip = strip.custom(strip.names = TRUE, var.name = "Depth"))

show.settings()
