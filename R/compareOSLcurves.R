library(Luminescence)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/articulos/OSL properties of heavy minerals/sec_bin")
#cargar archivo .binx y leerlo
sample = "OSL-TL_garnet-kyanite_R2_050219.binx"
sample1 = "OSL-TL_garnet-kyanite_R2_050219.binx"
#sample2 = "Dating_L0673_R1_160906.binx"
#sample3 = "Dating_L0677-678_R1_170526.binx"

sample = read_BIN2R(sample)
sample1 = read_BIN2R(sample1)
#sample2 = read_BIN2R(sample2)
#sample3 = read_BIN2R(sample3)

al1 = 3
al2 = 7

#convertir .binx a RLum, indicar numero de alicuotas
sample = Risoe.BINfileData2RLum.Analysis(sample, pos = al1, run = 5)
sample1 = Risoe.BINfileData2RLum.Analysis(sample1, pos = al2, run = 5)
#sample2 = Risoe.BINfileData2RLum.Analysis(sample2, pos = 1)
#sample3 = Risoe.BINfileData2RLum.Analysis(sample3, pos = 23)

norm.sample = sample$IRSL@data[,2]#/max(sample3$OSL[1,2])
norm.sample1 = sample1$OSL@data[,2]#/max(sample3$OSL[1,2])
#norm.sample2 = sample2[1,1][,2]#/max(sample3$OSL[1,2])
#norm.sample3 = sample3[1,1][,2]#/max(sample3$OSL[1,2])
#"norm.sample4 = sample4$OSL[[9]][,2]#/max(sample1$OSL[[24]][,2])
t1 = sample1$OSL@data[,1]
#t2 = sample1[[23]][,1]

#plot curvas, colores "#4467a4","#f49d36", "#e94e1c", "#52b79e"
png("OSL-TL.jpg", width = 700, height = 500)
par(mar = c(5,5,1.5,1.5))
matplot(x = t1, y = cbind(norm.sample, norm.sample1),
        col = c("red","blue"), xlim = c(0, 100), ylim = c(0,max(norm.sample)), lty = 1, type = "l", lwd = 2,
        xlab = "", ylab = "OSL (cts/0.1 s)", xaxt = "n", yaxs = "i", xaxs = "i",
        cex.lab = 2.5, cex.axis = 1.8)

axis(side = 1, seq(0,100,20), cex.axis = 1.8)
mtext(side = 1, text = "Stimulation Time (s)", cex = 2.5, line = 2.7)

legend("topright", c("Almandine", "Kyanite"), 
       col = c("red","blue"), lty = 1, cex = 2)
dev.off()
