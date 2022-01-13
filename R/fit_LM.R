library(Luminescence)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/proyectos/Parnaiba/sec_bin/")
#cargar archivo .binx y leerlo
#sample = "TL_LM-OSL_garnet-kyanite_R2_230119.binx"
sample = "SensParnaiba_primeirolote_150209.binx"
#sample = "Dating_L0674_R1_160910.binx"

sample = read_BIN2R(sample)

aliquot = 1
#bg = 17
n.components = 3

signal = Risoe.BINfileData2RLum.Analysis(sample, pos = aliquot, run = 5)
time = signal[[1]][,1]
signal = signal[[1]][,2]
signal = data.frame(time, signal)
#bg = Risoe.BINfileData2RLum.Analysis(sample, pos = bg, run = 5)
#bg = data.frame(bg[[1]][,1], bg[[1]][,2])

#matplot(y = cbind(signal[,2], bg[,2]), type = "l")

#png("Lm-OSL.jpg", width = 700, height = 1000)
fit = fit_LMCurve(signal, n.components = n.components, fit.method = "port", 
                  #start_values = data.frame(Im = c(134, 73, 130619), xm = c(149, 881, 3537976)),
                  fit.calcError = F, fit.advanced = F, fit.trace = F,
                  xlim = c(1, 40), #ylim = c(0,500), log = "x", 
                  LED.power = 40, LED.wavelength = 470,
                  #bg.subtraction = "polynomial", plot.BG = F,
                  input.dataType = "pLM", main = "", cex = 1.4)
#dev.off()

cont = data.frame(fit$component.contribution.matrix)
signal = data.frame(signal)
time = cont[,2]; comp1 = cont[,8]/100*signal[,2]; comp2 = cont[,9]/100*signal[,2]; comp3 = cont[,14]/100*signal[,2]#; comp4 = cont[,15]/100*signal[,2]
#png("LM-OSL.jpg", width = 700, height = 600)
par(mar = c(4.5,4.5,2,1.5))
matplot(time, cbind(comp1, comp2, comp3), type = "l", lwd = 2, lty = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
        cex = 1, cex.lab = 2, cex.axis = 2, log = "x", xaxs = "i", yaxs = "i", xlim = c(1.25, 5000), ylim = c(0,max(signal$signal)),
        col = c("#4467a4","#f49d36", "#e94e1c", "#52b79e"))
par(new = T, mar = c(4.5,4.5,2,1.5))
matplot(signal[,1], signal[,2], type = "l", xlab = "Time (s)", ylab = "LM-OSL (cts/1.25 s)",
        cex = 1, cex.lab = 2, cex.axis = 2, log = "x", xaxs = "i", yaxs = "i", xlim = c(1.25, 5000), ylim = c(0,max(signal$signal)))
#legend("topright", #title = "Almandine", 
       legend = c("Sum", "Comp1", "Comp2", "Comp3", "Comp4"),
       col = c("black", "#4467a4","#f49d36", "#e94e1c", "#52b79e"),
       lwd = 2, cex = 1.6)
#dev.off()

