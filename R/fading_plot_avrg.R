rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Resultados")
#cargar los datos
sample = "L0478"
data = read_excel("fading test.xlsx", sheet = 5, col_names = T)

time_50 = data[seq(1,23,4),3]
time_290 = data[seq(3,25,4),3]
time_50_norm = time_50/time_50[1]
time_290_norm = time_290/time_290[1]

signal50_1 = data[seq(1,23,4),8]
bg50_1 = data[seq(1,23,4),9]
tsig50_1 = data[seq(2,24,4),8]
tbg50_1 = data[seq(2,24,4),9]
lxtx_50_1 = (signal50_1-(bg50_1/5))

signal50_3 = data[seq(1,23,4),14]
bg50_3 = data[seq(1,23,4),15]
tsig50_3 = data[seq(2,24,4),14]
tbg50_3 = data[seq(2,24,4),15]
lxtx_50_3 = (signal50_3-(bg50_3/5))

signal50_5 = data[seq(1,23,4),20]
bg50_5 = data[seq(1,23,4),21]
tsig50_5 = data[seq(2,24,4),20]
tbg50_5 = data[seq(2,24,4),21]
lxtx_50_5 = (signal50_5-(bg50_5/5))

lxtx_50_1_norm = lxtx_50_1/lxtx_50_1[1]
lxtx_50_3_norm = lxtx_50_3/lxtx_50_3[1]
lxtx_50_5_norm = lxtx_50_5/lxtx_50_5[1]
lxtx_50 = (lxtx_50_1_norm+lxtx_50_3_norm+lxtx_50_5_norm)/3

matplot(log10(time_50_norm), cbind(lxtx_50),
        xlab = "log10(time) (h)",
        ylab = "lxtx norm",
        pch = c(15),
        #log = "x",
        #xlim = c(1000,300000),
        #ylim = c(0,1.5),
        #xaxt = "n",
        col = c("red"),
        main = c(paste("Sample", sample, "IR 50")))
#axis(1, c(1000,10000,100000))

lm_50 = lm((lxtx_50)~log10(time_50_norm))

abline(lm_50, col = "red")

legend("topright",
       pch = c(15),
       col = c("red"),
       legend = c(paste(signif((coefficients(lm_50)[2]*-100), 3), "%/decade")))

signal290_1 = data[seq(3,25,4),8]
bg290_1 = data[seq(3,25,4),9]
tsig290_1 = data[seq(4,26,4),8]
tbg290_1 = data[seq(4,26,4),9]
lxtx_290_1 = (signal290_1-(bg290_1/5))

signal290_3 = data[seq(3,25,4),14]
bg290_3 = data[seq(3,25,4),15]
tsig290_3 = data[seq(4,26,4),14]
tbg290_3 = data[seq(4,26,4),15]
lxtx_290_3 = (signal290_3-(bg290_3/5))

signal290_5 = data[seq(3,25,4),20]
bg290_5 = data[seq(3,25,4),21]
tsig290_5 = data[seq(4,26,4),20]
tbg290_5 = data[seq(4,26,4),21]
lxtx_290_5 = (signal290_5-(bg290_5/5))

lxtx_290_1_norm = lxtx_290_1/lxtx_290_1[1]
lxtx_290_3_norm = lxtx_290_3/lxtx_290_3[1]
lxtx_290_5_norm = lxtx_290_5/lxtx_290_5[1]
lxtx_290 = (lxtx_290_1_norm+lxtx_290_3_norm+lxtx_290_5_norm)/3

matplot(log10(time_290_norm), cbind(lxtx_290),
        xlab = "log10(time) (h)",
        ylab = "lxtx norm",
        pch = c(15),
        #log = "x",
        #xlim = c(1000,300000),
        #ylim = c(0,1.5),
        #xaxt = "n",
        col = c("red"),
        main = c(paste("Sample", sample, "pIR 290")))
#axis(1, c(1000,10000,100000))

lm_290 = lm((lxtx_290)~log10(time_290_norm))

abline(lm_290, col = "red")

legend("topright",
       pch = c(15),
       col = c("red"),
       legend = c(paste(signif((coefficients(lm_290)[2]*-100), 3), "%/decade")))