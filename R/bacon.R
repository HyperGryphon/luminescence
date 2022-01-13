library(rbacon); library(readxl)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/luminiscencia/Pampa Argentina - Renata")
#cargar archivo .xlsx y leerlo

Bacon(core = "lz", sep = ";", d.min = 150, d.max = 400, cc = 0,
      acc.shape = 0.1, acc.mean = 500, mem.strength = 4, mem.mean = 0.7,
      thick = 6)
