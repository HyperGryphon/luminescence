library(Luminescence)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados/sec_bin")
#cargar archivo .binx y leerlo
sample = "Dating_L0672_R1_160902.binx"
sample1 = sample

sample = read_BIN2R(sample)

aliquot = 3
run = 4
dose.rate = sample@METADATA$IRR_DOSERATE[1]

#convertir .binx a RLum, indicar numero de alicuotas
sample = Risoe.BINfileData2RLum.Analysis(sample, pos = aliquot)

detplot = plot_DetPlot(sample, signal.integral.min = 1, signal.integral.max = 20, 
                       background.integral.min = 310, background.integral.max = 410)