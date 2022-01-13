rm(list = ls())
setwd("/mnt/c/Users/iande/Documents/Proyectos/Atacama - Tesis/sec_bin")
#cargar los datos
sample = "L0478_tx"
data.tx = read.table(file = paste(sample,".txt", sep = ""), header = T, fill = T, sep = "\t")

tn = data.tx$signal[1]
tx = data.tx$signal[2:7]

bgtn = data.tx$bg[1]
bgtx = data.tx$bg[2:7]

txtn = (tx-bgtx)/(tn-bgtn)

plot(txtn,
     pch = 15,
     ylim = c(0,1.4),
     xlab = "SAR cycles",
     ylab = "Tx/Tn")

abline(h = c(0.9, 1, 1.1),
       lty = 2:1)
