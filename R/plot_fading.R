library(Luminescence)
library(Hmisc)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Resultados/sec_bin")
#cargar archivo .binx y leerlo
sample = "Fading_pIR290_L0536_R2.binx"

sample = read_BIN2R(sample)

aliquot = 7
dose.rate = sample@METADATA$IRR_DOSERATE[aliquot]
time.since.irr.ir = sample@METADATA$TIMESINCEIRR
time.since.irr.post = sample@METADATA$TIMESINCEIRR

sample.ir = subset(sample, TEMPERATURE == 50)
#sample.ir = subset(sample, LTYPE == "IRSL")
post.sample = subset(sample, TEMPERATURE == 290)
#post.sample = subset(sample, LTYPE == "OSL")

#convertir .binx a RLum, indicar numero de alicuotas
sample.ir = Risoe.BINfileData2RLum.Analysis(sample.ir, pos = aliquot)
post.sample = Risoe.BINfileData2RLum.Analysis(post.sample, pos = aliquot)

#analizar secuencia pIRIR definiendo canales a usar, background y la secuencia
sar = analyse_SAR.CWOSL(sample.ir, 
                        signal.integral.min = 1,
                        signal.integral.max = 20,
                        background.integral.min = 1910,
                        background.integral.max = 2000,
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = F)
post.sar = analyse_SAR.CWOSL(post.sample, 
                        signal.integral.min = 1,
                        signal.integral.max = 20,
                        background.integral.min = 1910,
                        background.integral.max = 2000,
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = F)

#dev.off()
par(mar = c(4.5,4.5,4,3))
dosegy = sar@data$LnLxTnTx.table[[3]] * dose.rate
dosegy.post = post.sar@data$LnLxTnTx.table[[3]] * dose.rate
LxTx.Data = as.data.frame(cbind(dosegy, sar@data$LnLxTnTx.table[c(12, 13, 6)]))
LxTx.Data.post = as.data.frame(cbind(dosegy.post, post.sar@data$LnLxTnTx.table[c(12, 13, 6)]))

xlim = c(0,1000)
ylim = c(0, max(LxTx.Data[2])+0.2*max(LxTx.Data[2]))


par(mar = c(4.5,4.5,2,1.5))
plot(y = LxTx.Data[2:9,2], x = time.since.irr.ir[,1],
     ylab = "", xlab = "", type = "p",
     #xlim = c(0, xlim), ylim = c(0, max(LxTx.Data.post[2])+0.2*max(LxTx.Data.post[2])),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T, mar = c(4.5,4.5,2,1.5))
errbar(LxTx.Data[1:7,1], LxTx.Data[1:7,2], ylab = "", xlab = "", pch = 16,
       yplus = LxTx.Data[1:7,2]+LxTx.Data[1:7,3], yminus = LxTx.Data[1:7,2]-LxTx.Data[1:7,3],
       #xlim = c(1, 9), ylim = c(0, max(LxTx.Data.post[2])+0.2*max(LxTx.Data.post[2])),
       xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T, mar = c(4.5,4.5,2,1.5))
plot(y = LxTx.Data.post[2:9,2], x = time.since.irr.post[,1],
     ylab = "", xlab = "", type = "p",
     #xlim = c(0, x), ylim = c(0, max(LxTx.Data.post[2])+0.2*max(LxTx.Data.post[2])),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n")
par(new = T, mar = c(4.5,4.5,2,1.5))
errbar(LxTx.Data.post[1:7,1], LxTx.Data.post[1:7,2], pch = 17,
       yplus = LxTx.Data.post[1:7,2]+LxTx.Data.post[1:7,3], yminus = LxTx.Data.post[1:7,2]-LxTx.Data.post[1:7,3],
       xlab = "Dose (Gy)", ylab = expression("L"[x]*"/T"[x]),
       #xlim = c(1, 9), ylim = c(0, max(LxTx.Data.post[2])+0.2*max(LxTx.Data.post[2])),
       xaxs = "i", yaxs = "i", cex.lab = 1.2, cex.axis = 1.2)
