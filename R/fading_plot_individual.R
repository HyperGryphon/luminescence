library(Luminescence)
library(readxl)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados/sec_bin")
#cargar los datos
sample = "Fading_L0478.binx"
data = read_BIN2R(sample)

aliquot = 1

dose_rate = data@METADATA$IRR_DOSERATE[aliquot]

data.ir = subset(data, TEMPERATURE == 50)
data.post = subset(data, TEMPERATURE == 290)

data.ir = Risoe.BINfileData2RLum.Analysis(data.ir, pos = aliquot)
data.post = Risoe.BINfileData2RLum.Analysis(data.post, pos = aliquot)



time_50 = data.frame(data.ir[[3]]$TIMESINCEIRR,
                     data.ir[[5]]$TIMESINCEIRR,
                     data.ir[[7]]$TIMESINCEIRR,
                     data.ir[[9]]$TIMESINCEIRR,
                     data.ir[[11]]$TIMESINCEIRR,
                     data.ir[[13]]$TIMESINCEIRR,
                     data.ir[[15]]$TIMESINCEIRR,
                     data.ir[[17]]$TIMESINCEIRR)
time_290 = data.frame(data.post[[3]]$TIMESINCEIRR,
                     data.post[[5]]$TIMESINCEIRR,
                     data.post[[7]]$TIMESINCEIRR,
                     data.post[[9]]$TIMESINCEIRR,
                     data.post[[11]]$TIMESINCEIRR,
                     data.post[[13]]$TIMESINCEIRR,
                     data.post[[15]]$TIMESINCEIRR,
                     data.post[[17]]$TIMESINCEIRR)
time_50_norm = time_50/time_50[[1]]
time_290_norm = time_290/time_290[[1]]

signal50 = data.frame(sum(data.ir[[3]]@data[1:100,2]),
                        sum(data.ir[[5]]@data[1:100,2]),
                        sum(data.ir[[7]]@data[1:100,2]),
                        sum(data.ir[[9]]@data[1:100,2]),
                        sum(data.ir[[11]]@data[1:100,2]),
                        sum(data.ir[[13]]@data[1:100,2]),
                        sum(data.ir[[15]]@data[1:100,2]),
                        sum(data.ir[[17]]@data[1:100,2]))
bg50_1 = data[seq(1,23,4),9]
tsig50_1 = data[seq(2,24,4),8]
tbg50_1 = data[seq(2,24,4),9]
lxtx_50 = (signal50-(bg50_1/5))/(tsig50_1-(tbg50_1/5))

signal50_3 = data[seq(1,23,4),14]
bg50_3 = data[seq(1,23,4),15]
tsig50_3 = data[seq(2,24,4),14]
tbg50_3 = data[seq(2,24,4),15]
lxtx_50_3 = (signal50_3-(bg50_3/5))/(tsig50_3-(tbg50_3/5))

signal50_5 = data[seq(1,23,4),20]
bg50_5 = data[seq(1,23,4),21]
tsig50_5 = data[seq(2,24,4),20]
tbg50_5 = data[seq(2,24,4),21]
lxtx_50_5 = (signal50_5-(bg50_5/5))/(tsig50_5-(tbg50_5/5))

lxtx_50_1_norm = lxtx_50_1/lxtx_50_1[[1,1]]
lxtx_50_3_norm = lxtx_50_3/lxtx_50_3[[1,1]]
lxtx_50_5_norm = lxtx_50_5/lxtx_50_5[[1,1]]
#lxtx_50 = (lxtx_50_1_norm+lxtx_50_3_norm+lxtx_50_5_norm)/3

matplot(log10(time_50_norm), cbind(lxtx_50_1_norm,
                                   lxtx_50_3_norm,
                                   lxtx_50_5_norm),
        xlab = "log10(time) (h)",
        ylab = "lxtx norm",
        pch = c(15, 17, 3),
        #log = "x",
        #xlim = c(1000,300000),
        #ylim = c(0,1.5),
        #xaxt = "n",
        col = c("red", "blue", "gray"),
        main = c(paste("Sample", sample, "IR 50")))
#axis(1, c(1000,10000,100000))

lm_50_1 = lm((lxtx_50_1_norm)~log10(time_50_norm))
lm_50_3 = lm((lxtx_50_3_norm)~log10(time_50_norm))
lm_50_5 = lm((lxtx_50_5_norm)~log10(time_50_norm))

abline(lm_50_1, col = "red")
abline(lm_50_3, col = "blue")
abline(lm_50_5, col = "gray")

legend("topright",
       pch = c(15, 17, 19),
       col = c("red", "blue", "gray"),
       legend = c(paste(signif((coefficients(lm_50_1)[2]*-100), 3), "%/decade"),
                  paste(signif((coefficients(lm_50_3)[2]*-100), 3), "%/decade"),
                  paste(signif((coefficients(lm_50_5)[2]*-100), 3), "%/decade")),
       cex = 0.9)

signal290_1 = data[seq(3,25,4),8]
bg290_1 = data[seq(3,25,4),9]
tsig290_1 = data[seq(4,26,4),8]
tbg290_1 = data[seq(4,26,4),9]
lxtx_290_1 = (signal290_1-(bg290_1/5))/(tsig290_1-(tbg290_1/5))

signal290_3 = data[seq(3,25,4),14]
bg290_3 = data[seq(3,25,4),15]
tsig290_3 = data[seq(4,26,4),14]
tbg290_3 = data[seq(4,26,4),15]
lxtx_290_3 = (signal290_3-(bg290_3/5))/(tsig290_3-(tbg290_3/5))

signal290_5 = data[seq(3,25,4),20]
bg290_5 = data[seq(3,25,4),21]
tsig290_5 = data[seq(4,26,4),20]
tbg290_5 = data[seq(4,26,4),21]
lxtx_290_5 = (signal290_5-(bg290_5/5))/(tsig290_5-(tbg290_5/5))

lxtx_290_1_norm = lxtx_290_1/lxtx_290_1[[1,1]]
lxtx_290_3_norm = lxtx_290_3/lxtx_290_3[[1,1]]
lxtx_290_5_norm = lxtx_290_5/lxtx_290_5[[1,1]]
#lxtx_290 = (lxtx_290_1_norm+lxtx_290_3_norm+lxtx_290_5_norm)/3

matplot(log10(time_290_norm), cbind(lxtx_290_1_norm,
                                    lxtx_290_3_norm,
                                    lxtx_290_5_norm),
        xlab = "log10(time) (h)",
        ylab = "lxtx norm",
        pch = c(15),
        #log = "x",
        #xlim = c(1000,300000),
        #ylim = c(0,1.5),
        #xaxt = "n",
        col = c("red", "blue", "gray"),
        main = c(paste("Sample", sample, "pIR 290")))
#axis(1, c(1000,10000,100000))

lm_290_1 = lm((lxtx_290_1_norm)~log10(time_290_norm))
lm_290_3 = lm((lxtx_290_3_norm)~log10(time_290_norm))
lm_290_5 = lm((lxtx_290_5_norm)~log10(time_290_norm))

abline(lm_290_1, col = "red")
abline(lm_290_3, col = "blue")
abline(lm_290_5, col = "gray")

legend("topright",
       pch = c(15, 17, 19),
       col = c("red", "blue", "gray"),
       legend = c(paste(signif((coefficients(lm_290_1)[2]*-100), 3), "%/decade"),
                  paste(signif((coefficients(lm_290_3)[2]*-100), 3), "%/decade"),
                  paste(signif((coefficients(lm_290_5)[2]*-100), 3), "%/decade")),
       cex = 0.9)