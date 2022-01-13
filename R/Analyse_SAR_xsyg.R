library(Luminescence)
library(Hmisc)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/luminiscencia/Atacama - Tesis/sec_bin")
#cargar archivo .binx y leerlo
sample = "Dating_L0670_R2.binx"
sample1 = sample

sample = read_BIN2R(sample)

aliquot = 3
run = 11
dose.rate = sample@METADATA$IRR_DOSERATE[1]
sample = subset(sample, sample@METADATA$TEMPERATURE == 125)

#convertir .binx a RLum, indicar numero de alicuotas
sample = Risoe.BINfileData2RLum.Analysis(sample, pos = aliquot)

#pdf(file = paste(sample, "_", aliquot, ".pdf", sep = ""), height = 15, width = 15)

#analizar secuencia pIRIR definiendo canales a usar, background y la secuencia
sar = analyse_SAR.CWOSL(sample,
                        signal.integral.min = c(1,1),
                        signal.integral.max = c(8,8),
                        background.integral.min = c(310,310),
                        background.integral.max = c(400,400),
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
                               fit.force_through_origin = F,
                               fit.includingRepeatedRegPoints = F,
                               xlab = "Dose (Gy)", ylab = expression("L"[x]*"/T"[x]), 
                               xlim = c(0, 2600), ylim = c(0, max(LxTx.Data[2])+0.1*max(LxTx.Data[2])),
                               output.plot = T,
                               output.plotExtended = F,
                               output.plotExtended.single = T, cex = 1,
                               col = "red",
                               legend = F)

De.data = data.frame(De = sar$data$De*dose.rate, De.Error = sar$data$De.Error*dose.rate, DosD0 = sar$data$D01*dose.rate*0.86*2)
gc = coef(growthcurve$Fit)
#growthcurve components of the equation: a y D0
gca = gc[[1]]
gcb = gc[[2]]
xlim = 60
ylim = max(LxTx.Data[2])+0.2*max(LxTx.Data[2])
par(mar = c(4.5,4.5,2,1.5))
plot(y = gca*(1-exp(-(1:2600)/gcb)), x = c(1:2600),
     ylab = "", xlab = "", type = "l", lwd = 2,
     xlim = c(0,xlim), ylim = c(0, ylim),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T, mar = c(4.5,4.5,2,1.5))
errbar(LxTx.Data[1:11,1], LxTx.Data[1:11,2],
       yplus = LxTx.Data[1:11,2]+LxTx.Data[1:11,3], yminus = LxTx.Data[1:11,2]-LxTx.Data[1:11,3],
       xlab = "Dose (Gy)", ylab = expression("L"[x]*"/T"[x]), cex = 1.2,
       xlim = c(0,xlim), ylim = c(0, ylim),
       xaxs = "i", yaxs = "i", cex.lab = 1.2, cex.axis = 1.2)
par(new = T)
segments(x0 = 0, x1 = De.data$De, y0 = LxTx.Data$LxTx[1], y1 = LxTx.Data$LxTx[1], col = "black", lwd = 2)
segments(x0 = De.data$De, x1 = De.data$De, y0 = 0, y1 = LxTx.Data$LxTx[1], col = "black", lwd = 2)

#plotear curva OSL como inset
par(new = T, fig = c(0.4,0.97,0.12,0.6))
s = sample$OSL[[1]]@data[,2]
t = sample$OSL[[1]]@data[,1]
par(mar = c(1,4.5,4.5,1.5))
plot(x = t, y = s, xlab = "", ylab = "OSL (cts/0.1 s)", xaxt = "n", yaxt = "n", main = "", type = "l",
     xaxs = "i", yaxs = "i", xlim = c(0,40), ylim = c(0, max(s)), cex.lab = 1.2)
axis(2, at = seq(0,max(s),100), cex.axis = 1.2)
#mtext(side = 2, "OSL (cts/0.1 s)", line = 2.2, cex = 1.5)
axis(3, at = seq(0,40,10), cex.axis = 1.2)
mtext(side = 3, "Stimulation time (s)", line = 2.2, cex = 1.2)

sar$rejection.criteria[c(1,2,4)]
IR.dep.ratio = (((t(sar$LnLxTnTx.table$LnLx)[8]-t(sar$LnLxTnTx.table$LnLx.BG)[8])/
                   (t(sar$LnLxTnTx.table$TnTx)[7]-t(sar$LnLxTnTx.table$TnTx.BG)[7]))/
                  ((t(sar$LnLxTnTx.table$LnLx)[7]-t(sar$LnLxTnTx.table$LnLx.BG)[7])/
                   (t(sar$LnLxTnTx.table$TnTx)[7]-t(sar$LnLxTnTx.table$TnTx.BG)[7])))
IRSL.BLSL.ratio = sum(sample[15]$IRSL[1:4])/sum(sample[16]$OSL[1:4])*100

aliquot = aliquot
run = run
fr = read_BIN2R(sample1)
fr = Risoe.BINfileData2RLum.Analysis(fr, pos = aliquot, run = run)
#sigmaF y sigmaM son la photoionisation cross-section de las componentes rápida y media
fr = calc_FastRatio(fr, stimulation.power = 40, wavelength = 470,
                    sigmaF = 3.57e-17,
                    sigmaM = 4.68e-18, plot = F)
fr = get_RLum(fr)

data.frame(De.data$De, De.data$De.Error)
data.frame(IR.dep.ratio, IRSL.BLSL.ratio)
data.frame(fr$fast.ratio, fr$fast.ratio.se)