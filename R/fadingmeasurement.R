library(Luminescence)
library(readxl)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados")
#cargar archivo .binx y leerlo
name = "L0674 225"
sample = read_excel("Fading test.xlsx" , sheet = name, col_names = T)

lxtx = na.omit(sample$lxtx[3:8])
lxtx.err = na.omit(sample$lxtx.err[3:8])
t = na.omit(sample$t[3:8])

data = data.frame(lxtx, lxtx.err, t)

analyse_FadingMeasurement(data, 
                          plot.single = T)
