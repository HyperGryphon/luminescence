rm(list = ls())
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones")
#cargar los datos
data = read_excel("tabla resumen.xlsx", sheet = 1, col_names = T)

ir50 = data[,16]
pir290 = data[,12]

plot(pir290, ir50,
     xlim = c(0,400),
     ylim = c(0,400))

abline(a = 1,
       b = 1)