library(Luminescence)
library(Hmisc)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documentos/proyectos/Purus/binfiles/")
#cargar archivo .binx y leerlo
sample = "Dating_L1415_R2_191220.binx"
sample1 = sample

sample0 = read_BIN2R(sample) #n = 198

aliquot = 47
run = 3
dose.rate = sample0@METADATA$IRR_DOSERATE[1]
sample = subset(sample0, sample0@METADATA$TEMPERATURE == 125)# & sample@METADATA$RUN>8)

#convertir .binx a RLum, indicar numero de alicuotas
sample = Risoe.BINfileData2RLum.Analysis(sample0, pos = aliquot)

#pdf(file = paste(sample, "_", aliquot, ".pdf", sep = ""), height = 15, width = 15)

#analizar secuencia pIRIR definiendo canales a usar, background y la secuencia
sar = analyse_SAR.CWOSL(sample,
                        signal.integral.min = c(1,1),
                        signal.integral.max = c(8,8),
                        background.integral.min = c(300,300),
                        background.integral.max = c(399,399),
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = F)

#dev.off()
par(mar = c(4.5,4.5,4,2))
dosegy = sar@data$LnLxTnTx.table[[3]] * dose.rate
LxTx.Data = as.data.frame(cbind(dosegy, sar@data$LnLxTnTx.table[c(12, 13, 6)]))
growthcurve = plot_GrowthCurve(LxTx.Data[1:8,], main = "", mtext = "",
                               fit.method = "EXP",
                               fit.weights = F,
                               fit.force_through_origin = T,
                               fit.includingRepeatedRegPoints = F,
                               xlab = "Dose (Gy)", ylab = expression("L"[x]*"/T"[x]), 
                               xlim = c(0, 2600), ylim = c(0, max(LxTx.Data[2])+0.1*max(LxTx.Data[2])),
                               output.plot = F,
                               output.plotExtended = F,
                               output.plotExtended.single = T, cex = 1,
                               col = "red",
                               legend = F)

De.data = data.frame(De = sar$data$De*dose.rate, De.Error = sar$data$De.Error*dose.rate, D0 = sar$data$D01*dose.rate)
gc = coef(growthcurve$Fit)
#growthcurve components of the equation: a y D0
gca = gc[[1]]
gcb = gc[[2]]
#gcc = gc[[3]]
#gcd = gc[[4]]
xlim = max(LxTx.Data[,1])+0.2*max(LxTx.Data[,1])
ylim = max(LxTx.Data[,2])+0.2*max(LxTx.Data[,2])

setwd("C:/Users/iande/Documentos/proyectos/Purus/")
#pdf("drc L1202.pdf", width = 10, height = 8)
par(mar = c(4.5,4.8,2,1.5))
plot(y = gca*(1-exp(-(0:500)/gcb)), #+gcc*(1-exp(-(1:2600)/gcd)), 
     x = c(0:500),
     ylab = "", xlab = "", type = "l", lwd = 2,
     xlim = c(0,xlim), ylim = c(0, ylim),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T)
errbar(LxTx.Data[1:11,1], LxTx.Data[1:11,2],
       yplus = LxTx.Data[1:11,2]+LxTx.Data[1:11,3], yminus = LxTx.Data[1:11,2]-LxTx.Data[1:11,3],
       xlab = "Dose (Gy)", ylab = expression("L"[x]*"/T"[x]), cex = 1.5,
       xlim = c(0,xlim), ylim = c(0, ylim),
       xaxs = "i", yaxs = "i", cex.lab = 2, cex.axis = 2)
par(new = T)
legend("topleft", legend = c(paste0("Sample L1202"), paste0("De = ", round(De.data$De, 1), " \u00B1 ", round(De.data$De.Error,1), " Gy"),
                             paste0("D0 = ", round(De.data$D0, 0), " Gy")),
       cex = 1.5)
#líneas de interpolación
segments(x0 = 0, x1 = De.data$De, y0 = LxTx.Data$LxTx[1], y1 = LxTx.Data$LxTx[1], col = "red", lwd = 3, lty = 2)
segments(x0 = De.data$De, x1 = De.data$De, y0 = 0, y1 = LxTx.Data$LxTx[1], col = "red", lwd = 3, lty = 2)

#plotear curva OSL como inset
par(new = T)
s1 = sample$OSL[[1]]@data[,2]
s2 = sample$OSL[[3]]@data[,2]
t = sample$OSL[[1]]@data[,1]
par(mar = c(5,24,24,2.1))
plot(x = t, y = s1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "", type = "l", lwd = 2, lty = 1,
     xaxs = "i", yaxs = "i", xlim = c(0,40), ylim = c(0, max(s1,s2)+max(s1,s2)*0.05), cex.lab = 1.5, col = "black")
par(new = T, mar = c(5,24,24,2.1))
plot(x = t, y = s2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "", type = "l", lwd = 2, lty = 1,
     xaxs = "i", yaxs = "i", xlim = c(0,40), ylim = c(0, max(s1,s2)+max(s1,s2)*0.05), cex.lab = 1.5, col = "red")
axis(2, at = seq(0,max(s1,s2),50), cex.axis = 1.2)
mtext(side = 2, "OSL (cts/0.1 s)", line = 2.2, cex = 1.5)
axis(3, at = seq(0,40,10), cex.axis = 1.2)
mtext(side = 3, "Stimulation time (s)", line = 2.2, cex = 1.5)
legend("topright", legend = c("Natural", paste0("Reg 1: ", round(dosegy[2], 1), " Gy")), lwd = 2,
       col = c(1,2))

#dev.off()

sar$rejection.criteria[c(1,2,4)]
IR.dep.ratio = (((t(sar$LnLxTnTx.table$LnLx)[8]-t(sar$LnLxTnTx.table$LnLx.BG)[8])/
                   (t(sar$LnLxTnTx.table$TnTx)[7]-t(sar$LnLxTnTx.table$TnTx.BG)[7]))/
                  ((t(sar$LnLxTnTx.table$LnLx)[7]-t(sar$LnLxTnTx.table$LnLx.BG)[7])/
                   (t(sar$LnLxTnTx.table$TnTx)[7]-t(sar$LnLxTnTx.table$TnTx.BG)[7])))
IRSL.BLSL.ratio = sum(sample[15]$IRSL[1:4])/sum(sample[16]$OSL[1:4])*100

data.frame(De = De.data$De, De.err = De.data$De.Error, D0 = De.data$D0,
           Recycling = sar$rejection.criteria$Value[1], Recuperation = sar$rejection.criteria$Value[3]*100,
           IR.dep.ratio, IRSL.BLSL.ratio)