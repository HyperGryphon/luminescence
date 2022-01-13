rm(list = ls())
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones")
#cargar los datos
sample = "L0530"
data.gy = read.table(file = paste(sample,".txt", sep = ""), header = T, fill = T, sep = "\t")

#seleccionar solo las columnas de 50º y de 290º
data.gy50 = data.frame(data.gy$de50, data.gy$er50)
data.gy50 = na.omit(data.gy50)
data.gy290 = data.frame(data.gy$de290, data.gy$er290)
data.gy290 = na.omit(data.gy290)

#regresión lineal
r = lm(data.gy$de50~data.gy$de290)

plot(data.gy$de290, data.gy$de50,
     xlim = c(0,1000),
     ylim = c(0,300),
     #asp = 10,
     pch = 19)

#legend("topleft", legend = paste(summary(r$adj.r.squared)))
abline(r)

summary(r)