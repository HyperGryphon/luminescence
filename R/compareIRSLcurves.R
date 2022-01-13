library(Luminescence)
rm(list = ls())
#graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados/sec_bin")
#cargar archivo .binx y leerlo
sample = "Dating_pIR225_L0672_R2_180130.binx"

sample = read_BIN2R(sample)

al1 = 1

#convertir .binx a RLum, indicar numero de alicuotas
sample = Risoe.BINfileData2RLum.Analysis(sample, pos = al1)

norm.sample = sample$IRSL[[1]]@data[,2]#/max(sample$IRSL[[2]]@data[,2])
norm.sample.post = sample$IRSL[[2]]@data[,2]#/max(sample$IRSL[[2]]@data[,2])
t = sample$IRSL[[1]]@data[,1]

if(max(norm.sample) < max(norm.sample.post)){
  max = max(norm.sample.post)
} else{
  max = max(norm.sample)
}

par(mar = c(1,4.5,4,1.5))
plot(x = t, y = norm.sample, ylim = c(0,max), lty = 1, type = "l", lwd = 2,
     xlab = "", ylab = "", col = "black", yaxs = "i", xaxs = "i",
     cex.lab = 1.5, cex.axis = 1.5, xaxt = "n", yaxt = "n")
par(new = T)
plot(x = t, y = norm.sample.post, ylim = c(0,max), lty = 1, type = "l", lwd = 2,
     xlab = "", ylab = "IRSL (cts/0.1 s)", col = "#E9551F", yaxs = "i", xaxs = "i",
     cex.lab = 1.5, cex.axis = 1.5, xaxt = "n")

axis(3, at = seq(0,200,20), cex.axis = 1.5)
mtext(side = 3, "Stimulation Time (s)", line = 2.2, cex = 1.5)

