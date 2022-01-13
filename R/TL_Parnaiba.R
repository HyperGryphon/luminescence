library(Luminescence); library(gridExtra); library(ggplot2)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/proyectos/Parnaiba/sec_bin/")
#cargar archivo .binx y leerlo
sample1 = "SensParnaiba_primeirolote_150209.binx"
sample1 = read_BIN2R(sample1)
sample2 = "SensParnaiba_segundolote_150210.binx"
sample2 = read_BIN2R(sample2)
sample3 = "SensParnaiba_terceirolote_150211.binx"
sample3 = read_BIN2R(sample3)
sample4 = "SensParnaiba_quartolote_150212.binx"
sample4 = read_BIN2R(sample4)
sample5 = "SensParnaiba_quintolote_150213.binx"
sample5 = read_BIN2R(sample5)
sample6 = "SensParnaiba_sextolote_150219.binx"
sample6 = read_BIN2R(sample6)
sample7 = "SensParnaiba_setimolote_150220.binx"
sample7 = read_BIN2R(sample7)

aliquot = seq(1,47,2)

tl.1 = subset(sample1, sample1@METADATA$RUN == 6)
tl.1 = as.data.frame(tl.1@DATA)
tl.2 = subset(sample2, sample2@METADATA$RUN == 6)
tl.2 = as.data.frame(tl.2@DATA)
tl.3 = subset(sample3, sample3@METADATA$RUN == 6)
tl.3 = as.data.frame(tl.3@DATA)
tl.4 = subset(sample4, sample4@METADATA$RUN == 6)
tl.4 = as.data.frame(tl.4@DATA)
tl.5 = subset(sample5, sample5@METADATA$RUN == 6)
tl.5 = as.data.frame(tl.5@DATA)
tl.6 = subset(sample6, sample6@METADATA$RUN == 6)
tl.6 = as.data.frame(tl.6@DATA)
tl.7 = subset(sample7, sample7@METADATA$RUN == 6)
tl.7 = as.data.frame(tl.7@DATA)

tl.serra = subset(sample1, sample1@METADATA$POSITION == c(1,2,3,4) & sample2@METADATA$POSITION == c(1,2,3,4))

#calcular la media de los máximos de los picos y las sd asociadas
tl.1.mean = mean(apply(tl.1, 2, function(x) max(x, na.rm = TRUE)))
tl.1.sd = sd(apply(tl.1, 2, function(x) max(x, na.rm = TRUE)))
tl.2.mean = mean(apply(tl.2, 2, function(x) max(x, na.rm = TRUE)))
tl.2.sd = sd(apply(tl.2, 2, function(x) max(x, na.rm = TRUE)))
tl.3.mean = mean(apply(tl.3, 2, function(x) max(x, na.rm = TRUE)))
tl.3.sd = sd(apply(tl.3, 2, function(x) max(x, na.rm = TRUE)))
tl.4.mean = mean(apply(tl.4, 2, function(x) max(x, na.rm = TRUE)))
tl.4.sd = sd(apply(tl.4, 2, function(x) max(x, na.rm = TRUE)))
tl.5.mean = mean(apply(tl.5, 2, function(x) max(x, na.rm = TRUE)))
tl.5.sd = sd(apply(tl.5, 2, function(x) max(x, na.rm = TRUE)))
tl.6.mean = mean(apply(tl.6, 2, function(x) max(x, na.rm = TRUE)))
tl.6.sd = sd(apply(tl.6, 2, function(x) max(x, na.rm = TRUE)))
tl.7.mean = mean(apply(tl.7, 2, function(x) max(x, na.rm = TRUE)))
tl.7.sd = sd(apply(tl.7, 2, function(x) max(x, na.rm = TRUE)))

tl.serra = as.data.frame(c(tl.1[,1:44], tl.7[,37:48]))
tl.pimen = as.data.frame(c(tl.1[,45:48], tl.2[,1:32]))
tl.cabe = as.data.frame(c(tl.2[,33:48], tl.3[,1:28], tl.7[,21:36]))
tl.longa = as.data.frame(c(tl.3[,29:48], tl.4[,1:16]))
tl.poti = as.data.frame(c(tl.4[,17:48], tl.5[,1:24]))
tl.piaui = as.data.frame(c(tl.5[,25:48], tl.6[,1:16]))
tl.motuca = as.data.frame(c(tl.6[,17:36]))
tl.samba = as.data.frame(c(tl.6[,37:48], tl.7[,1:20]))

#calcular las T1/2 correspondientes a esos máximos y sd
#t12 = which.min(abs(tl[,1] - max(tl[,1]/2)) & abs(tl[,1] < max(tl[,1]/2)))*1.8
t12.1 = which.min(abs(tl.1[,1] - mean(tl.1[,1]/2)) & abs(tl.1[,1] < mean(tl.1[,1]/2)))*1.8
t12.2 = which.min(abs(tl.2[,1] - mean(tl.2[,1]/2)) & abs(tl.2[,1] < mean(tl.2[,1]/2)))*1.8
t12.3 = which.min(abs(tl.3[,1] - mean(tl.3[,1]/2)) & abs(tl.3[,1] < mean(tl.3[,1]/2)))*1.8
t12.4 = which.min(abs(tl.4[,1] - mean(tl.4[,1]/2)) & abs(tl.4[,1] < mean(tl.4[,1]/2)))*1.8
t12.5 = which.min(abs(tl.5[,1] - mean(tl.5[,1]/2)) & abs(tl.5[,1] < mean(tl.5[,1]/2)))*1.8
t12.6 = which.min(abs(tl.6[,1] - mean(tl.6[,1]/2)) & abs(tl.6[,1] < mean(tl.6[,1]/2)))*1.8
t12.7 = which.min(abs(tl.7[,1] - mean(tl.7[,1]/2)) & abs(tl.7[,1] < mean(tl.7[,1]/2)))*1.8
tl.mean = data.frame("mean" = c(tl.1.mean, tl.2.mean, tl.3.mean, tl.4.mean, tl.5.mean, tl.6.mean, tl.7.mean),
                     "sd" = c(tl.1.sd, tl.2.sd, tl.3.sd, tl.4.sd, tl.5.sd, tl.6.sd, tl.7.sd),
                     "t12" = c(t12.1, t12.2, t12.3, t12.4, t12.5, t12.6, t12.7))

xlim = c(0,450); ylim = c(0,max(tl.5)+0.1*max(tl.5))
t = seq(1,450,1.80)

par(mar = c(4.5,4.8,2,1.5), mfrow=c(4,2))
matplot(y = cbind(tl.1), x = t, type = "l", lty = 1, col = "black", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = 1.2, cex.axis = 1.2, main = "Primeiro lote")
par(mar = c(4.5,4.8,2,1.5), new = F)
matplot(y = cbind(tl.2), x = t, type = "l", lty = 1, col = "red", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = 1.2, cex.axis = 1.2, main = "Segundo lote")
par(mar = c(4.5,4.8,2,1.5), new = F)
matplot(y = cbind(tl.3), x = t, type = "l", lty = 1, col = "blue", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = 1.2, cex.axis = 1.2, main = "Terceiro lote")
par(mar = c(4.5,4.8,2,1.5), new = F)
matplot(y = cbind(tl.4), x = t, type = "l", lty = 1, col = "green", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = 1.2, cex.axis = 1.2, main = "Quarto lote")
par(mar = c(4.5,4.8,2,1.5), new = F)
matplot(y = cbind(tl.5), x = t, type = "l", lty = 1, col = "purple", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = 1.2, cex.axis = 1.2, main = "Quinto lote")
par(mar = c(4.5,4.8,2,1.5), new = F)
matplot(y = cbind(tl.6), x = t, type = "l", lty = 1, col = "orange", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = 1.2, cex.axis = 1.2, main = "Sexto lote")
par(mar = c(4.5,4.8,2,1.5), new = F)
matplot(y = cbind(tl.7), x = t, type = "l", lty = 1, col = "grey", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)", 
        lwd = 1, cex.lab = 1.2, cex.axis = 1.2, main = "Sétimo lote")

setwd("C:/Users/iande/Documents/proyectos/Parnaiba/")
jpeg("TL_Parnaiba.jpeg", width = 1500, height = 1000, quality = 100)
cex.lab = 2; cex.axis = 1.6; cex.main = 2
par(mar = c(5,5.5,2.5,2), mfrow=c(4,2))
matplot(y = cbind(tl.serra), x = t, type = "l", lty = 1, col = "black", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Serra Grande")
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.pimen), x = t, type = "l", lty = 1, col = "red", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Pimenteiras")
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.cabe), x = t, type = "l", lty = 1, col = "blue", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Cabeças")
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.longa), x = t, type = "l", lty = 1, col = "green", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Longá")
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.poti), x = t, type = "l", lty = 1, col = "purple", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Poti")
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.piaui), x = t, type = "l", lty = 1, col = "orange", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Piaui")
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.motuca), x = t, type = "l", lty = 1, col = "grey", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)", 
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Motuca")
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.samba), x = t, type = "l", lty = 1, col = "pink", xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)", 
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Sambaiba")
dev.off()
