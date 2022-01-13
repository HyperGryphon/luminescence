library(Luminescence)
library(readxl)
library(Hmisc)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/luminiscencia/Atacama - Tesis/sec_bin")
setwd("C:/Users/iande/Documents/luminiscencia/Colombia - Mauricio/sec_bin")
#cargar archivo .binx y leerlo
sample = "2019-03-29_Dating L1199-1202.xsyg"

sample = read_BIN2R(sample)
sample = read_XSYG2R(sample)

aliquot = 1

dose.rate = sample@METADATA$IRR_DOSERATE[aliquot]

sample.ir = subset(sample, TEMPERATURE == 50)
#sample.ir = subset(sample, LTYPE == "IRSL")
post.sample = subset(sample, TEMPERATURE == 225)
#post.sample = subset(sample, LTYPE == "OSL")

#convertir .binx a RLum, indicar numero de alicuotas
sample.ir = Risoe.BINfileData2RLum.Analysis(sample.ir, pos = aliquot)
post.sample = Risoe.BINfileData2RLum.Analysis(post.sample, pos = aliquot)

#analizar secuencia pIRIR definiendo canales a usar, background y tipo de secuencia
sar = analyse_SAR.CWOSL(sample.ir, 
                        signal.integral.min = 1,
                        signal.integral.max = 20,
                        background.integral.min = 1850,
                        background.integral.max = 2000,
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = F)
post.sar = analyse_SAR.CWOSL(post.sample, 
                        signal.integral.min = 1,
                        signal.integral.max = 20,
                        background.integral.min = 1850,
                        background.integral.max = 2000,
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = F)

De.data = data.frame(De.50 = sar$data$De*dose.rate, De.err.50 = sar$data$De.Error*dose.rate)
De.data.post = data.frame(De.290 = post.sar$data$De*dose.rate, De.err.290 = post.sar$data$De.Error*dose.rate)

#dev.off()
par(new = F, mar = c(4.5,4.5,4,3))
dosegy = sar@data$LnLxTnTx.table[[3]] * dose.rate
dosegy.post = post.sar@data$LnLxTnTx.table[[3]] * dose.rate
LxTx.Data = as.data.frame(cbind(dosegy, sar@data$LnLxTnTx.table[c(12, 13, 6)]))
LxTx.Data.post = as.data.frame(cbind(dosegy.post, post.sar@data$LnLxTnTx.table[c(12, 13, 6)]))

xlim = c(0,800)
ylim = c(0, max(LxTx.Data[2])+0.2*max(LxTx.Data[2]))

par(lty = 1, lwd = 1)
growthcurve = plot_GrowthCurve(LxTx.Data, output.plot = F,
                               fit.method = "EXP",
                               fit.weights = T,
                               fit.includingRepeatedRegPoints = T,
                               xlab = "Dose (Gy)",
                               ylab = expression("L"[x]*"/T"[x]),
                               output.plotExtended = F,
                               xlim = xlim, ylim = ylim,
                               main = NULL, mtext = "")

#par(new = T, lty = 2, lwd = 1)
growthcurve.post = plot_GrowthCurve(LxTx.Data.post, output.plot = F,
                               fit.method = "EXP",
                               fit.weights = T,
                               fit.includingRepeatedRegPoints = T,
                               xlab = "",
                               ylab = "",
                               output.plotExtended = F,
                               xlim = xlim, ylim = ylim,
                               main = NULL, mtext = "")

gc = coef(growthcurve$Fit)
gc.post = coef(growthcurve.post$Fit)
#componentes de la curva de dosis-respuesta a y D0 / ojo a si se usa una doble exponencial
a50 = gc[[1]]
#a502 = gc[[3]]
b50 = gc[[2]]
#b502 = gc[[4]]
a290 = gc.post[[1]]
#a2902 = gc.post[[3]]
b290 = gc.post[[2]]
#b2902 = gc.post[[4]]
xlim = 1:400
ylim = max(LxTx.Data.post[2])+0.2*max(LxTx.Data.post[2])
par(mar = c(4.5,4.5,2,1.5))
plot(y = ((a50*(1-exp(-(xlim/b50)))))#+(a502*(1-exp(-(xlim/b502))))
     , x = c(xlim),
     ylab = "", xlab = "", type = "l", lty = 1, col = "black", lwd = 2,
     xlim = c(0, max(xlim)), ylim = c(0, ylim),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T, mar = c(4.5,4.5,2,1.5))
errbar(LxTx.Data[1:7,1], LxTx.Data[1:7,2], ylab = "", xlab = "", pch = 16,
       yplus = LxTx.Data[1:7,2]+LxTx.Data[1:7,3], yminus = LxTx.Data[1:7,2]-LxTx.Data[1:7,3],
       xlim = c(0, max(xlim)), ylim = c(0, ylim),
       xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T, mar = c(4.5,4.5,2,1.5))
plot(y = (a290*(1-exp(-(xlim/b290))))#+(a2902*(1-exp(-(xlim/b2902))))
     , x = c(xlim),
     ylab = "", xlab = "", type = "l", lty = 1, col = "#E9551F", lwd = 2,
     xlim = c(0, max(xlim)), ylim = c(0, ylim),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T, mar = c(4.5,4.5,2,1.5))
errbar(LxTx.Data.post[1:7,1], LxTx.Data.post[1:7,2], pch = 17,
       yplus = LxTx.Data.post[1:7,2]+LxTx.Data.post[1:7,3], yminus = LxTx.Data.post[1:7,2]-LxTx.Data.post[1:7,3],
       xlab = "Dose (Gy)", ylab = expression("L"[x]*"/T"[x]),
       xlim = c(0, max(xlim)), ylim = c(0, ylim),
       xaxs = "i", yaxs = "i", cex.lab = 1.2, cex.axis = 1.2)
par(new = T)
segments(x0 = 0, x1 = De.data$De.50, y0 = LxTx.Data$LxTx[1], y1 = LxTx.Data$LxTx[1], col = "black", lwd = 2)
segments(x0 = De.data$De.50, x1 = De.data$De.50, y0 = 0, y1 = LxTx.Data$LxTx[1], col = "black", lwd = 2)
segments(x0 = 0, x1 = De.data.post$De.290, y0 = LxTx.Data.post$LxTx[1], y1 = LxTx.Data.post$LxTx[1],
         col = "#E9551F", lwd = 2)
segments(x0 = De.data.post$De.290, x1 = De.data.post$De.290, y0 = 0, y1 = LxTx.Data.post$LxTx[1],
         col = "#E9551F", lwd = 2)

#plotear inset comparación entre curvas IRSL 
par(new = T, fig = c(0.4,0.97,0.12,0.6))
norm.sample = sample.ir[[2]]@data[,2]#/max(sample.ir[[2]]@data[,2])
norm.sample.post = post.sample[[2]]@data[,2]#/max(sample.ir[[2]]@data[,2])
t1 = sample.ir[[1]]@data[,1]
t2 = post.sample[[1]]@data[,1]
if(max(norm.sample) < max(norm.sample.post)){
  max = max(norm.sample.post)
} else{
  max = max(norm.sample)
}
par(mar = c(1,4.5,4,1.5))
plot(x = t1, y = norm.sample, ylim = c(0,max), lty = 1, type = "l", lwd = 2,
     xlab = "", ylab = "", col = "black", yaxs = "i", xaxs = "i",
     cex.lab = 1.2, cex.axis = 1.2, xaxt = "n", yaxt = "n")
par(new = T)
plot(x = t2, y = norm.sample.post, ylim = c(0,max), lty = 1, type = "l", lwd = 2,
     xlab = "", ylab = "IRSL (cts/0.1 s)", col = "#E9551F", yaxs = "i", xaxs = "i",
     cex.lab = 1.2, cex.axis = 1.2, xaxt = "n")
axis(3, at = seq(0,200,20), cex.axis = 1.2)
mtext(side = 3, "Stimulation Time (s)", line = 2.2, cex = 1.2)

#post.sar$rejection.criteria[c(1,2,3,4)]
De.data
De.data.post
#growthcurve$Fit
#growthcurve.post$Fit
sar$rejection.criteria