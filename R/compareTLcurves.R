library(Luminescence)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documentos/proyectos/Purus/binfiles//")
#cargar archivo .binx y leerlo
sample = "MULTI_TL_stability_TEST_1416-17_R2_191218.binx"
tl.nat = "Pulse_Anneal_TL_L1415-16-17-18_R2_191210.binx"
sample = read_BIN2R(sample)
tl.after = read_BIN2R(tl.nat)
tl.nat = read_BIN2R(tl.nat)

#21,23, 45,47
aliquot = 23
#1,9,17,25
aliquot.tl.nat = 9
aliquot.tl.after = 9

tl = subset(sample, sample@METADATA$LTYPE == "TL" 
            & sample@METADATA$IRR_TIME == 300 
            & aliquot ==  aliquot & sample@METADATA$AN_TEMP > 160)
tl.test = subset(sample, sample@METADATA$LTYPE == "TL" 
                 & sample@METADATA$IRR_TIME == 100 
                 & aliquot == aliquot 
                 & sample@METADATA$AN_TEMP == 160)
tl.nat = subset(tl.nat, tl.nat@METADATA$LTYPE == "TL" 
                & tl.nat@METADATA$POSITION == aliquot.tl.nat
                & tl.nat@METADATA$RUN == 3)
tl.after = subset(tl.after, tl.after@METADATA$LTYPE == "TL" 
                  & tl.after@METADATA$POSITION == aliquot.tl.after
                  & tl.after@METADATA$RUN == 75)
tl.after = as.data.frame(tl.after@DATA)
tl.nat = as.data.frame(tl.nat@DATA)

#convertir .binx a RLum, indicar numero de alicuotas
tl = Risoe.BINfileData2RLum.Analysis(tl, aliquot)
tl.test = Risoe.BINfileData2RLum.Analysis(tl.test, aliquot)
i = 1:8
setClass("RLum.Analysis", representation(x = "numeric", y = "numeric"))
setAs("RLum.Analysis", "data.frame", function(from) data.frame(tl[[i]]))
setAs("RLum.Analysis", "data.frame", function(from) data.frame(tl.test[[i]]))
tl = as.data.frame(tl[[i]])
tl.test = as.data.frame(tl.test[[i]])
tl = tl[0:215,]; tl.test = tl.test[0:215,]; 
tl.temp = tl[,seq(1,2*max(i),2)]; tl.lum = tl[,seq(2,2*max(i),2)]; 
tl.temp.test =  tl.test[,seq(1,2*max(i),2)]; tl.test = tl.test[,seq(2,2*max(i),2)]
xlim = c(0,450); ylim = c(0,max(tl.after)+0.1*max(tl.after))
t = seq(1,450,1.80)

par(mar = c(4.5,4.8,2,1.5))
matplot(tl.temp, tl.lum, type = "l", lty = 1, col = rainbow(8), xlim = xlim, ylim = ylim,
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = c(1,1,1,1,1,1,1,1,1,2))
par(new = T, mar=c(4.5,4.8,2,1.5))
matplot(t, cbind(tl.nat, tl.after), xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
        type = "l", xlab = "", ylab = "", lty = 1, col = c("black", "black"), lwd = c(1,2))
legend(x = 300, y = max(tl.after), legend = c("180 ºC", "200 ºC", "220 ºC", "240 ºC", "260 ºC", "280 ºC", "300 ºC", "320 ºC","Natural","After thermal test"),
       lty = 1, lwd = c(1,1,1,1,1,1,1,1,1,2), col = c(rainbow(8), "black", "black"))
matplot(tl.temp.test, tl.test, type = "l", lty = 1, col = rainbow(8), xlim = c(0,160), ylim = c(0,max(tl.test)+0.1*max(tl.test)),
        xaxs = "i", yaxs = "i", xlab = "Temperature (ºC)", ylab = "TL intensity (cts)",
        lwd = c(1,1,1,1,1,1,1,1,1,2))
legend(x = 10, y = max(tl.test), legend = c("180 ºC", "200 ºC", "220 ºC", "240 ºC", "260 ºC", "280 ºC", "300 ºC", "320 ºC"),
       lty = 1, col = rainbow(8))
