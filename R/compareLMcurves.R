library(Luminescence)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/luminiscencia/Atacama - tesis/sec_bin")
#cargar archivo .binx y leerlo
sample = "LM_5000_L0672-673-680_R2_170531.binx"

sample = read_BIN2R(sample)

al1 = 3
al2 = 5
al3 = 11

#convertir .binx a RLum, indicar numero de alicuotas
sample1 = Risoe.BINfileData2RLum.Analysis(sample, pos = al1, run = 7)
sample2 = Risoe.BINfileData2RLum.Analysis(sample, pos = al2, run = 7)
sample3 = Risoe.BINfileData2RLum.Analysis(sample, pos = al3, run = 7)

norm.sample1 = sample1$RBR[,2]#/max(sample1$RBR[,2])
t = (sample1[[1]][,1])
norm.sample2 = sample2$RBR[,2]#/max(sample2$RBR[,2])
norm.sample3 = sample3$RBR[,2]#/max(sample3$RBR[,2])

png("compare LM-OSL curves.jpg", width = 750, height = 500)
par(mar = c(4.5,4.5,2,1.6))
plot(x = t, y = norm.sample2, lty = 1, type = "l", lwd = 1, cex.axis = 1.8, cex.lab = 2.2,
     xlab = "Stimulation time (s)", ylab = "LM-OSL (cts/1.25 s)", col = "white", yaxs = "i", xaxs = "i",
     xlim = c(1,5000), log ="x", xaxt = "n")
axis(1, at = c(1, 10, 100, 1000,5000), cex.axis = 1.8)
par(new = T, mar = c(4.5,4.5,2,1.5))
lines(smooth.spline(x = t, y = norm.sample2, df = 150),col = "#f14c14", lwd = 3)
par(new = T, mar = c(4.5,4.5,2,1.5))
#plot(x = t, y = norm.sample3, ylim = c(0,1), lty = 1, type = "l", lwd = 1,
#     xlab = "", ylab = "", col = "#53b79e", axes = F, yaxs = "i", xaxs = "i")
lines(smooth.spline(x = t, y = norm.sample3, df = 150),col = "#53b79e", lwd = 3)
par(new = T, mar = c(4.5,4.5,2,1.5))
#plot(x = t, y = norm.sample1, ylim = c(0,1), lty = 1, type = "l", lwd = 1,
#     xlab = "", ylab = "", col = "#f39c35", yaxs = "i", xaxs = "i")
lines(smooth.spline(x = t, y = norm.sample1, df = 150),col = "#f39c35", lwd = 3)
dev.off()
#legend(3700,120, lwd = 4, c("L0672", "L0673", "L0680"), lty = 1, c("#f14c14", "#53b79e", "#f39c35" ))