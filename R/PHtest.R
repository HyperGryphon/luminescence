library(Hmisc)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados")
#cargar archivo .binx y leerlo
sample1 = "L0480_PH2.txt"
sample2 = "L0674_PH2.txt"
sample3 = "L0677_PH2.txt"
data1 = read.table(file = paste(sample1), header = T, fill = T, sep = "\t")
data2 = read.table(file = paste(sample2), header = T, fill = T, sep = "\t")
data3 = read.table(file = paste(sample3), header = T, fill = T, sep = "\t")
dose1 = 87
dose2 = 50
dose3 = 48.5

par(mar = c(4.5,4.5,2,1.5))
errbar(data1$t, data1$De/dose1, data1$De/dose1+data1$De.Err/dose1, data1$De/dose1-data1$De.Err/dose1, 
     ylab = "Calculated-to-given dose", cex = 1.8, cex.axis = 1.6, cex.lab = 2,
     xlab = "Preheat temperature (ºC)",
     ylim = c(0.6,1.6), xlim = c(170, 270), pch = 19, col = "gray")
par(new = T, mar = c(4.5,4.5,2,1.5))
errbar(data2$t-2, data2$De/dose2, data2$De/dose2+data2$De.Err/dose2, data2$De/dose2-data2$De.Err/dose2, 
       ylab = "", yaxt = "n", cex = 1.2,
       xlab = "", xaxt = "n",
       ylim = c(0.6,1.6), xlim = c(170, 270), pch = 19, col = "gray51")
par(new = T,mar = c(4.5,4.5,2,1.5))
errbar(data3$t+2, data3$De/dose3, data3$De/dose3+data3$De.Err/dose3, data3$De/dose3-data3$De.Err/dose3, 
       ylab = "", yaxt = "n", cex = 1.2,
       xlab = "", xaxt = "n",
       ylim = c(0.6,1.6), xlim = c(170, 270), pch = 19, col = "gray0")
abline(h = 1, lty = 1)
abline(h = 1.1, lty = 5)
abline(h = 0.9, lty = 5)
#legend("topleft", pch = c(19,19,19), col = c("gray", "gray51", "gray0"),
#       legend = c("L0480  ", "L0674  ", "L0677  "),
#       bty = "n")