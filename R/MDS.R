#multidimensional scaling analysis

library(xlsx); library(ggplot2); library(MASS)
setwd("G:/Mi unidad/Parnaiba/")

data = read.xlsx("osl_parnaiba_table.xlsx", sheetIndex = 2, header = T)

samples = data$Sample
units = data$Unit
osl = data$osl
irsl = data$irsl
tl325pos = data$tl325pos
tl110 = data$tl110
tl110pos = data$tl110pos

all = data.frame(samples, osl, irsl, tl325pos, tl110, tl110pos)
d = dist(all[,-1])
fit = cmdscale(d, eig = T, k = 2) #classical MDS
#fit = isoMDS(d, k = 2) #non-metric MDS
fit

x <- fit$points[,1]
y <- fit$points[,2]
points = data.frame(x,y)
ggplot(points, aes(x, y, group = units))+
  geom_point(size = 3, aes(shape = units, color = units))+
  scale_shape_manual(values = c(15:18,15:18))+
  scale_colour_manual(values = rainbow(8))+
  labs(x = "Cordinate 1", y = "Cordinate 2")+
  geom_text(aes(label = units), hjust = 0, vjust = 0)+
  theme_classic()
