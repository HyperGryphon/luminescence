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

#serra 14 muestras
tl.serra.bp = as.data.frame(c(
        tl.bp75a = tl.1[,1:4],
        tl.bp86 = tl.1[,5:8],
        tl.bp89 = tl.1[,9:12],
        tl.bp92 = tl.1[,13:16],
        tl.bp100 = tl.1[,17:20],
        tl.bp106 = tl.1[,21:24],
        tl.bp204 = tl.1[,25:28],
        tl.bp207 = tl.1[,29:32],
        tl.bp211 = tl.1[,33:36],
        tl.bp214 = tl.1[,37:40],
        tl.bp244 = tl.1[,41:44],
        tl.bp87 = tl.7[,37:40],
        tl.bp102 = tl.7[,41:44],
        tl.bp104a = tl.7[,45:48]))

#pimen 9 muestras
tl.pimen.bp = as.data.frame(c(
        tl.bp98 = tl.1[,45:48],
        tl.bp165 = tl.2[,1:4],
        tl.bp171 = tl.2[,5:8],
        tl.bp181 = tl.2[,9:12],
        tl.bp184 = tl.2[,13:16],
        tl.bp203 = tl.2[,17:20],
        tl.bp208 = tl.2[,21:24],
        tl.bp210a = tl.2[,25:28],
        tl.bp219 = tl.2[,29:32]))

#cabe 15 muestras
tl.cabe.bp = as.data.frame(c(
        tl.bp72 = tl.2[,33:36],
        tl.bp79 = tl.2[,37:40],
        tl.bp94 = tl.2[,41:44],
        tl.bp174 = tl.2[,45:48],
        tl.bp177 = tl.3[,1:4],
        tl.bp182 = tl.3[,5:8],
        tl.bp189 = tl.3[,9:12],
        tl.bp194 = tl.3[,13:16],
        tl.bp202 = tl.3[,17:20],
        tl.bp210b = tl.3[,21:24],
        tl.bp220 = tl.3[,25:28],
        tl.bp78 = tl.7[21:24],
        tl.bp81a = tl.7[,25:28],
        tl.bp96 = tl.7[,29:32],
        tl.bp175 = tl.7[,33:36]))

#longa 9 muestras
tl.longa.bp = as.data.frame(c(
        tl.bp83 = tl.3[,29:32],
        tl.bp114 = tl.3[,33:36],
        tl.bp172 = tl.3[,37:40],
        tl.bp173 = tl.3[,41:44],
        tl.bp179 = tl.3[,45:48],
        tl.bp180 = tl.4[,1:4],
        tl.bp195 = tl.4[,5:8],
        tl.bp198 = tl.4[,9:12],
        tl.bp223 = tl.4[,13:16]))

#poti 14 muestras
tl.poti.bp = as.data.frame(c(
        tl.bp82a = tl.4[,17:20],
        tl.bp82e = tl.4[,21:24],
        tl.bp82f = tl.4[,25:28],
        tl.bp82g = tl.4[,29:32],
        tl.bp82h = tl.4[,33:36],
        tl.bp82i = tl.4[,37:40],
        tl.bp131 = tl.4[,41:44],
        tl.bp132 = tl.4[,45:48],
        tl.bp136 = tl.5[,1:4],
        tl.bp138 = tl.5[,5:8],
        tl.bp140 = tl.5[,9:12],
        tl.bp154 = tl.5[,13:16],
        tl.bp226 = tl.5[,17:20],
        tl.bp238 = tl.5[,21:24]))

#piaui 10 muestras
tl.piaui.bp = as.data.frame(c(
        tl.bp63 = tl.5[,25:28],
        tl.bp68 = tl.5[,29:32],
        tl.bp125 = tl.5[,33:36],
        tl.bp139 = tl.5[,37:40],
        tl.bp141 = tl.5[,41:44],
        tl.bp150 = tl.5[,45:48],
        tl.bp64 = tl.6[,1:4],
        tl.bp67 = tl.6[,5:8],
        tl.bp146b = tl.6[,9:12],
        tl.bp155 = tl.6[,13:16]))

#motuca 5 muestras
tl.motuca.bp = as.data.frame(c(
        tl.bp133 = tl.6[,17:20],
        tl.bp135 = tl.6[,21:24],
        tl.bp143 = tl.6[,25:28],
        tl.bp144 = tl.6[,29:32],
        tl.185 = tl.6[,33:36]))

#samba 8 muestras
tl.samba.bp = as.data.frame(c(
        tl.bp120a = tl.6[37:40],
        tl.bp121a = tl.6[41:44],
        tl.bp121b = tl.6[,44:48],
        tl.bp121c = tl.7[,1:4],
        tl.bp121d = tl.7[,5:8],
        tl.bp121f = tl.7[,9:12],
        tl.bp121h = tl.7[,13:16],
        tl.bp123 = tl.7[,17:20]))

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

xlim = c(0,450); #ylim = c(0,max(tl.5)+0.1*max(tl.5))
t = seq(1,450,1.80)

setwd("C:/Users/iande/Documents/proyectos/Parnaiba/")
jpeg("TL_Parnaiba.jpeg", width = 1500, height = 1000, quality = 100)
cex.lab = 2; cex.axis = 1.6; cex.main = 2
par(mar = c(5,5.5,2.5,2), mfrow=c(4,2))
matplot(y = cbind(tl.serra.bp), x = t, type = "l", lty = 1, col = rainbow(14), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Serra Grande")
#dev.off()
#jpeg("TL_pimen.jpeg", width = 1500, height = 1000, quality = 100)
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.pimen.bp), x = t, type = "l", lty = 1, col = rainbow(9), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Pimenteiras")
#dev.off()
#jpeg("TL_cabe.jpeg", width = 1500, height = 1000, quality = 100)
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.cabe.bp), x = t, type = "l", lty = 1, col = rainbow(15), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Cabeças")
#dev.off()
#jpeg("TL_longa.jpeg", width = 1500, height = 1000, quality = 100)
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.longa.bp), x = t, type = "l", lty = 1, col = rainbow(9), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Longá")
#dev.off()
#jpeg("TL_poti.jpeg", width = 1500, height = 1000, quality = 100)
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.poti.bp), x = t, type = "l", lty = 1, col = rainbow(14), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Poti")
#dev.off()
#jpeg("TL_piaui.jpeg", width = 1500, height = 1000, quality = 100)
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.piaui.bp), x = t, type = "l", lty = 1, col = rainbow(10), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Piaui")
#dev.off()
#jpeg("TL_motuca.jpeg", width = 1500, height = 1000, quality = 100)
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.motuca.bp), x = t, type = "l", lty = 1, col = rainbow(5), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)", 
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Motuca")
#dev.off()
#jpeg("TL_samba.jpeg", width = 1500, height = 1000, quality = 100)
par(mar = c(5,5.5,2.5,2), new = F)
matplot(y = cbind(tl.samba.bp), x = t, type = "l", lty = 1, col = rainbow(8), xlim = xlim, #ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)", 
        lwd = 1, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main, main = "Sambaiba")
dev.off()
