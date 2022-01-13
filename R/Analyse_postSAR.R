library(Luminescence)
library(readxl)
library(Hmisc)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documentos/proyectos/services/maria chile/binfiles/")
#cargar archivo .binx y leerlo
sample = "TestDose_pIR290_L1518-19-20.binx"
sample = read_BIN2R(sample)

name = "L1520 - SA1"

setwd("C:/Users/iande/Documentos/proyectos/services/maria chile")
aliquot = 11
pdf(paste0("DRC sample ",name,"_",aliquot,".pdf"), height = 8, width = 10)

xlim = 1:500

dose.rate = sample@METADATA$IRR_DOSERATE[aliquot]

sample.ir = subset(sample, TEMPERATURE == 50)
#sample.ir = subset(sample, LTYPE == "IRSL")
post.sample = subset(sample, TEMPERATURE == 290)
#post.sample = subset(sample, LTYPE == "OSL")

#convertir .binx a RLum, indicar numero de alicuotas
sample.ir = Risoe.BINfileData2RLum.Analysis(sample.ir, pos = aliquot)
post.sample = Risoe.BINfileData2RLum.Analysis(post.sample, pos = aliquot)

#analizar secuencia pIRIR definiendo canales a usar, background y tipo de secuencia
sar = analyse_SAR.CWOSL(sample.ir, 
                        signal.integral.min = 1,
                        signal.integral.max = 20,
                        background.integral.min = 1801,
                        background.integral.max = 2000,
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = F)
post.sar = analyse_SAR.CWOSL(post.sample, 
                        signal.integral.min = 1,
                        signal.integral.max = 20,
                        background.integral.min = 1801,
                        background.integral.max = 2000,
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = F)


#dev.off()
par(new = F, mar = c(4.5,4.5,4,3))
dosegy = sar@data$LnLxTnTx.table[[3]] * dose.rate
dosegy.post = post.sar@data$LnLxTnTx.table[[3]] * dose.rate
LxTx.Data = as.data.frame(cbind(dosegy, sar@data$LnLxTnTx.table[c(12, 13, 6)]))
LxTx.Data.post = as.data.frame(cbind(dosegy.post, post.sar@data$LnLxTnTx.table[c(12, 13, 6)]))

ylim = c(0, max(LxTx.Data[2])+0.2*max(LxTx.Data[2]))

par(lty = 1, lwd = 1)
growthcurve = plot_GrowthCurve(LxTx.Data, output.plot = F,
                               fit.method = "EXP",
                               fit.weights = T,
                               fit.includingRepeatedRegPoints = T,
                               fit.force_through_origin = T,
                               xlab = "Dose (Gy)",
                               ylab = expression("L"[x]*"/T"[x]),
                               output.plotExtended = F,
                               xlim = c(0,800), ylim = ylim,
                               main = NULL, mtext = "")

#par(new = T, lty = 2, lwd = 1)
growthcurve.post = plot_GrowthCurve(LxTx.Data.post, output.plot = F,
                               fit.method = "EXP",
                               fit.weights = T,
                               fit.includingRepeatedRegPoints = T,
                               fit.force_through_origin = T,
                               xlab = "",
                               ylab = "",
                               output.plotExtended = F,
                               xlim = c(0,800), ylim = ylim,
                               main = NULL, mtext = "")

De.data = data.frame(De.50 = growthcurve$De$De, De.err.50 = growthcurve$De$De.Error)
De.data.post = data.frame(De.290 = growthcurve.post$De$De, De.err.290 = growthcurve.post$De$De.Error)

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
ylim = max(LxTx.Data.post[2], LxTx.Data[2])+0.2*max(LxTx.Data.post[2], LxTx.Data[2])
par(mar = c(5,5,1.5,1.5))
plot(y = ((a50*(1-exp(-(xlim/b50)))))#+(a502*(1-exp(-(xlim/b502))))
     , x = c(xlim),
     ylab = "", xlab = "", type = "l", lty = 1, col = "black", lwd = 2,
     xlim = c(0, max(xlim)), ylim = c(0, ylim),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n", main = name, cex.main = 2)
par(new = T)
errbar(LxTx.Data[1:7,1], LxTx.Data[1:7,2], ylab = "", xlab = "", pch = 16, cex = 2,
       yplus = LxTx.Data[1:7,2]+LxTx.Data[1:7,3], yminus = LxTx.Data[1:7,2]-LxTx.Data[1:7,3],
       xlim = c(0, max(xlim)), ylim = c(0, ylim),
       xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T)
plot(y = (a290*(1-exp(-(xlim/b290))))#+(a2902*(1-exp(-(xlim/b2902))))
     , x = c(xlim),
     ylab = "", xlab = "", type = "l", lty = 1, col = "red", lwd = 2,
     xlim = c(0, max(xlim)), ylim = c(0, ylim),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T)
errbar(LxTx.Data.post[1:7,1], LxTx.Data.post[1:7,2], pch = 17, cex = 2,
       yplus = LxTx.Data.post[1:7,2]+LxTx.Data.post[1:7,3], yminus = LxTx.Data.post[1:7,2]-LxTx.Data.post[1:7,3],
       xlab = "Dose (Gy)", ylab = expression("L"[x]*"/T"[x]),
       xlim = c(0, max(xlim)), ylim = c(0, ylim),
       xaxs = "i", yaxs = "i", cex.lab = 1.5, cex.axis = 1.5)
par(new = T)
segments(x0 = 0, x1 = De.data$De.50, y0 = LxTx.Data$LxTx[1], y1 = LxTx.Data$LxTx[1], col = "black", lwd = 2, lty = 2)
segments(x0 = De.data$De.50, x1 = De.data$De.50, y0 = 0, y1 = LxTx.Data$LxTx[1], col = "black", lwd = 2, lty = 2)
segments(x0 = 0, x1 = De.data.post$De.290, y0 = LxTx.Data.post$LxTx[1], y1 = LxTx.Data.post$LxTx[1],
         col = "red", lwd = 2, lty = 2)
segments(x0 = De.data.post$De.290, x1 = De.data.post$De.290, y0 = 0, y1 = LxTx.Data.post$LxTx[1],
         col = "red", lwd = 2, lty = 2)
#abline(h = LxTx.Data.post$LxTx[1], col = "red", lwd = 2, lty = 2)
#abline(h = LxTx.Data$LxTx[1], col = "black", lwd = 2, lty = 2)
legend("topleft", legend = c(paste0("pIRIR290 De = ", ifelse(is.nan(De.data.post$De.290),
                                                             paste0("Saturated"), 
                                                             paste0(round(De.data.post$De.290, 1), " Gy"))),#\u00B1 ", round(De.data.post$De.err.290,1), " Gy"))),
                             paste0("IR50 De = ", paste0(round(De.data$De.50, 1), " Gy"))),#\u00B1 ", round(De.data$De.err.50,1), " Gy")),
       lty = 1, lwd = 2, col = c("red","black"), cex = 1.5, bty = "n")
#legend("bottomright", legend = c(paste0("pIRIR290 D0 = ", round(2*b290, 0), " Gy"), 
#                             paste0("IR50 D0 = ", round(2*b50, 0), " Gy")), 
#       cex = 1.5, bty = "n")

#plotear inset comparación entre curvas IRSL 
dose = unique(sample@METADATA$IRR_TIME*sample@METADATA$IRR_DOSERATE)[2]
norm.sample = sample.ir[[2]]@data[,2]#/dose#/max(sample.ir[[2]]@data[,2])
norm.sample.post = post.sample[[2]]@data[,2]#/dose#/max(sample.ir[[2]]@data[,2])
t1 = sample.ir[[1]]@data[,1]
t2 = post.sample[[1]]@data[,1]

#setwd("C:/Users/iande/Documentos/proyectos/LASSD 2021/")
#pdf(paste0("irsl ",name,".pdf"), height = 5, width = 6)
par(new = T, mar = c(6,24,22,2))
#par(new = F, mar = c(5,5,1.5,1.5))
plot(x = t1, y = norm.sample, ylim = c(0,max(norm.sample.post,norm.sample)), lty = 1, type = "l", lwd = 2,
     xlab = "", ylab = "", col = "black", xaxt = "n", yaxt = "n")
par(new = T)
plot(x = t2, y = norm.sample.post, ylim = c(0,max(norm.sample.post,norm.sample)), lty = 1, type = "l", lwd = 1,
     xlab = "", ylab = "IRSL (cts)", col = "red", xaxt = "n",
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
axis(3, at = seq(0,200,40), cex.axis = 1.2)
mtext(side = 3, "Stimulation Time (s)", line = 2.2, cex = 1.2)
#mtext(side = 2, "IRSL (cts)", line = 2.2, cex = 1.2)
#legend("topright", legend = c(paste0("IR50"), paste0("pIRIR290")),
#       lty = 1, lwd = 2, col = c("black", "red"), bty = "n", cex = 2,
#       title = paste0("Sample ",name))

dev.off()

#post.sar$rejection.criteria[c(1,2,3,4)]
De.data
De.data.post
#growthcurve$Fit
#growthcurve.post$Fit
post.sar$rejection.criteria