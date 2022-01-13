rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones")
sample = read.table("L0478_qtz.txt", sep = "\t", header = T)

curvedata = data.frame((sample[,2]*0.134), sample[,7], sample[,8])

ltx = mean(sample[1,7], sample[7,7], sample[14,7], sample[21,7],
                sample[28,7], sample[35,7], sampl[42,7], sample[49,7],
                sample[56,7], sample[63,7])
ltxerr = mean(sample[1,8], sample[7,8], sample[14,8], sample[21,8],
                sample[28,8], sample[35,8], sampl[42,8], sample[49,8],
                sample[56,8], sample[63,8])

ED = calED(curvedata, c(ltx, ltxerr), model = "exp", origin = F)

fitGrowth(curvedata, model = "exp", origin = T)
legend("bottomright", legend = paste("De = ", signif(ED$ED[1,1], 5),"\U00B1",signif(ED$ED[1,2], 3),"Gy"))