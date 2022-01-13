library(Luminescence)
library(numOSL)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/proyectos/Parnaiba/sec_bin/")
#cargar archivo .binx y leerlo
sample = "SensParnaiba_primeirolote_150209.binx"

sample = read_BIN2R(sample)

al1 = 3 #L0672
al2 = 5 #L0673
al3 = 11 #L0680
ncomp = 3

#convertir .binx a RLum, indicar numero de alicuotas
sample1 = Risoe.BINfileData2RLum.Analysis(sample, pos = al1, run = 5)
#sample2 = Risoe.BINfileData2RLum.Analysis(sample, pos = al2, run = 7)
#sample3 = Risoe.BINfileData2RLum.Analysis(sample, pos = al3, run = 7)

sample = data.frame(sample1$RBR@data[,1], sample1$RBR@data[,2])

#lista de valores en columna (quitar el t0), tipo de curva: cw o lm
lm = decomp(sample, ncomp=ncomp, typ="cw", log = "x",
       control.args=list(maxiter=100), weight = T, outfile = "LM")


csv = read.csv("LM.csv", sep = ",", header = T)

bg = csv$Background/csv$Comp.Sum*100
comp1 = (csv$Comp.1/csv$Comp.Sum*100)
comp2 = (csv$Comp.2/csv$Comp.Sum*100)
comp3 = (csv$Comp.3/csv$Comp.Sum*100)+bg
comp4 = (csv$Comp.4/csv$Comp.Sum*100)
comp5 = (csv$Comp.5*100/csv$Comp.Sum)
power = csv$Time*0.01
time = csv$Time

#salvar jpg a 1000x850
#png("LM-OSL.png", width = 1000, height = 850)
par(mar = c(5,5,2,2.5))
plot(time, csv$Comp.Sum, log = "x", type = "l", lty = 1, lwd = 2, xlim = c(1,100), ylim = c(0,1500),
     xlab = expression("Stimulation time (s)"), #Power density (mW/cm"*{}^2*")"), 
     ylab = "LM-OSL signal (a.u.)", xaxs = "i", yaxs = "i")
#axis(1, c(0.1, 10, 100, 1000), cex.axis = 1.8)
#axis(1, c(0.1, 1, 5, 10, 20, 40), cex.axis = 1.8)
matplot(x = csv$Time, y = cbind(comp1, comp2, comp3, comp4, comp5),
        type = "l", lwd = 2, lty = c(1,2,3,4,5), col = "black",
        xlim = c(0, 100), ylim = c(0,100), xlab = expression("Stimulation time (s)"), #Power density (mW/cm"*{}^2*")"), 
        ylab = "% of bulk signal", #log = "x",
        cex.lab = 2, cex.axis = 1.8, yaxs = "i", xaxs = "i")#, xaxt = "n")
legend(600, 60, legend = c("Comp1", "Comp2", "Comp3"), 
       lty = c(1,2,3,4,5), lwd = 2, cex = 2, box.lty = 0, box.col = 0)
#axis(1, c(0.1, 1, 5, 10, 20, 40), cex.axis = 1.8)
#dev.off()
