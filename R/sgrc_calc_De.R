rm(list = ls())
graphics.off()

library(tidyverse); library(ggpubr); library(plotly)

setwd("C:/Users/iande/Documentos/proyectos/Purus/")

l1419 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1419"))
l1420 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1420"))
l1424 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1424"))
l1425 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1425"))
l1426 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1426"))
l1509 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1509"))
l1510 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1510"))
l1511 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1511"))
l1512 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1512"))
l1513 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1513"))
l1516 = tibble(readxl::read_excel("SGC_Purus_Carlos.xlsx", sheet = "l1516"))

data = data.frame(l1419,l1420,l1424,l1425,l1426,l1509,l1510,l1511,l1512,l1513,l1516)
samples = c(rep("l1419",6),rep("l1420",6),rep("l1424",6),rep("l1425",6),rep("l1426",6),rep("l1509",6),rep("l1510",6),rep("l1511",6),rep("l1512",6),rep("l1513",6),rep("l1516",6))
data = data[2:5,]

dose = c(rep(data$dose,6),rep(data$dose.1,6),rep(data$dose.2,6),rep(data$dose.3,6),rep(data$dose.4,6),rep(data$dose.5,6),rep(data$dose.6,6),rep(data$dose.7,6),rep(data$dose.8,6),rep(data$dose.9,6),rep(data$dose.10,6))
dose.test = c(rep(data$dose.test,6),rep(data$dose.test.1,6),rep(data$dose.test.2,6),rep(data$dose.test.3,6),rep(data$dose.test.4,6),rep(data$dose.test.5,6),rep(data$dose.test.6,6),rep(data$dose.test.7,6),rep(data$dose.test.8,6),rep(data$dose.test.9,6),rep(data$dose.test.10,6))

lxtx = c(data$lxtx1,data$lxtx2,data$lxtx3,data$lxtx4,data$lxtx5,data$lxtx6)
lxtx1 = c(data$lxtx1.1,data$lxtx2.1,data$lxtx3.1,data$lxtx4.1,data$lxtx5.1,data$lxtx6.1)
lxtx2 = c(data$lxtx1.2,data$lxtx2.2,data$lxtx3.2,data$lxtx4.2,data$lxtx5.2,data$lxtx6.2)
lxtx3 = c(data$lxtx1.3,data$lxtx2.3,data$lxtx3.3,data$lxtx4.3,data$lxtx5.3,data$lxtx6.3)
lxtx4 = c(data$lxtx1.4,data$lxtx2.4,data$lxtx3.4,data$lxtx4.4,data$lxtx5.4,data$lxtx6.4)
lxtx5 = c(data$lxtx1.5,data$lxtx2.5,data$lxtx3.5,data$lxtx4.5,data$lxtx5.5,data$lxtx6.5)
lxtx6 = c(data$lxtx1.6,data$lxtx2.6,data$lxtx3.6,data$lxtx4.6,data$lxtx5.6,data$lxtx6.6)
lxtx7 = c(data$lxtx1.7,data$lxtx2.7,data$lxtx3.7,data$lxtx4.7,data$lxtx5.7,data$lxtx6.7)
lxtx8 = c(data$lxtx1.8,data$lxtx2.8,data$lxtx3.8,data$lxtx4.8,data$lxtx5.8,data$lxtx6.8)
lxtx9 = c(data$lxtx1.9,data$lxtx2.9,data$lxtx3.9,data$lxtx4.9,data$lxtx5.9,data$lxtx6.9)
lxtx10 = c(data$lxtx1.10,data$lxtx2.10,data$lxtx3.10,data$lxtx4.10,data$lxtx5.10,data$lxtx6.10)
lxtx = c(lxtx,lxtx1,lxtx2,lxtx3,lxtx4,lxtx5,lxtx6,lxtx7,lxtx8,lxtx9,lxtx10)

lxtx.norm = c((data$lxtx1/max(data$lxtx1)),(data$lxtx2/max(data$lxtx2)),(data$lxtx3/max(data$lxtx3)),(data$lxtx4/max(data$lxtx4)),(data$lxtx5/max(data$lxtx5)),(data$lxtx6/max(data$lxtx6)))
lxtx.norm1 = c((data$lxtx1.1/max(data$lxtx1.1)),(data$lxtx2.1/max(data$lxtx2.1)),(data$lxtx3.1/max(data$lxtx3.1)),(data$lxtx4.1/max(data$lxtx4.1)),(data$lxtx5.1/max(data$lxtx5.1)),(data$lxtx6.1/max(data$lxtx6.1)))
lxtx.norm2 = c((data$lxtx1.2/max(data$lxtx1.2)),(data$lxtx2.2/max(data$lxtx2.2)),(data$lxtx3.2/max(data$lxtx3.2)),(data$lxtx4.2/max(data$lxtx4.2)),(data$lxtx5.2/max(data$lxtx5.2)),(data$lxtx6.2/max(data$lxtx6.2)))
lxtx.norm3 = c((data$lxtx1.3/max(data$lxtx1.3)),(data$lxtx2.3/max(data$lxtx2.3)),(data$lxtx3.3/max(data$lxtx3.3)),(data$lxtx4.3/max(data$lxtx4.3)),(data$lxtx5.3/max(data$lxtx5.3)),(data$lxtx6.3/max(data$lxtx6.3)))
lxtx.norm4 = c((data$lxtx1.4/max(data$lxtx1.4)),(data$lxtx2.4/max(data$lxtx2.4)),(data$lxtx3.4/max(data$lxtx3.4)),(data$lxtx4.4/max(data$lxtx4.4)),(data$lxtx5.4/max(data$lxtx5.4)),(data$lxtx6.4/max(data$lxtx6.4)))
lxtx.norm5 = c((data$lxtx1.5/max(data$lxtx1.5)),(data$lxtx2.5/max(data$lxtx2.5)),(data$lxtx3.5/max(data$lxtx3.5)),(data$lxtx4.5/max(data$lxtx4.5)),(data$lxtx5.5/max(data$lxtx5.5)),(data$lxtx6.5/max(data$lxtx6.5)))
lxtx.norm6 = c((data$lxtx1.6/max(data$lxtx1.6)),(data$lxtx2.6/max(data$lxtx2.6)),(data$lxtx3.6/max(data$lxtx3.6)),(data$lxtx4.6/max(data$lxtx4.6)),(data$lxtx5.6/max(data$lxtx5.6)),(data$lxtx6.6/max(data$lxtx6.6)))
lxtx.norm7 = c((data$lxtx1.7/max(data$lxtx1.7)),(data$lxtx2.7/max(data$lxtx2.7)),(data$lxtx3.7/max(data$lxtx3.7)),(data$lxtx4.7/max(data$lxtx4.7)),(data$lxtx5.7/max(data$lxtx5.7)),(data$lxtx6.7/max(data$lxtx6.7)))
lxtx.norm8 = c((data$lxtx1.8/max(data$lxtx1.8)),(data$lxtx2.8/max(data$lxtx2.8)),(data$lxtx3.8/max(data$lxtx3.8)),(data$lxtx4.8/max(data$lxtx4.8)),(data$lxtx5.8/max(data$lxtx5.8)),(data$lxtx6.8/max(data$lxtx6.8)))
lxtx.norm9 = c((data$lxtx1.9/max(data$lxtx1.9)),(data$lxtx2.9/max(data$lxtx2.9)),(data$lxtx3.9/max(data$lxtx3.9)),(data$lxtx4.9/max(data$lxtx4.9)),(data$lxtx5.9/max(data$lxtx5.9)),(data$lxtx6.9/max(data$lxtx6.9)))
lxtx.norm10 = c((data$lxtx1.10/max(data$lxtx1.10)),(data$lxtx2.10/max(data$lxtx2.10)),(data$lxtx3.10/max(data$lxtx3.10)),(data$lxtx4.10/max(data$lxtx4.10)),(data$lxtx5.10/max(data$lxtx5.10)),(data$lxtx6.10/max(data$lxtx6.10)))
lxtx.norm = c(lxtx.norm,lxtx.norm1,lxtx.norm2,lxtx.norm3,lxtx.norm4,lxtx.norm5,lxtx.norm6,lxtx.norm7,lxtx.norm8,lxtx.norm9,lxtx.norm10)

#estimate De#############################################################
loto = data.frame(l1419,l1420,l1424,l1425,l1426,l1509,l1510,l1511,l1512,l1513,l1516)[1,]
dose.test = c(loto$dose.test,loto$dose.test.1,loto$dose.test.2,loto$dose.test.3,loto$dose.test.4,loto$dose.test.5,loto$dose.test.6,loto$dose.test.7,loto$dose.test.8,loto$dose.test.9,loto$dose.test.10)
dose.test = c(rep(loto$dose.test,6),rep(loto$dose.test.1,6),rep(loto$dose.test.2,6),rep(loto$dose.test.3,6),rep(loto$dose.test.4,6),rep(loto$dose.test.5,6),rep(loto$dose.test.6,6),rep(loto$dose.test.7,6),rep(loto$dose.test.8,6),rep(loto$dose.test.9,6),rep(loto$dose.test.10,6))
samples = c(rep("l1419",6),rep("l1420",6),rep("l1424",6),rep("l1425",6),rep("l1426",6),rep("l1509",6),rep("l1510",6),rep("l1511",6),rep("l1512",6),rep("l1513",6),rep("l1516",6))

lxtx = c(loto$lxtx1,loto$lxtx2,loto$lxtx3,loto$lxtx4,loto$lxtx5,loto$lxtx6)
lxtx1 = c(loto$lxtx1.1,loto$lxtx2.1,loto$lxtx3.1,loto$lxtx4.1,loto$lxtx5.1,loto$lxtx6.1)
lxtx2 = c(loto$lxtx1.2,loto$lxtx2.2,loto$lxtx3.2,loto$lxtx4.2,loto$lxtx5.2,loto$lxtx6.2)
lxtx3 = c(loto$lxtx1.3,loto$lxtx2.3,loto$lxtx3.3,loto$lxtx4.3,loto$lxtx5.3,loto$lxtx6.3)
lxtx4 = c(loto$lxtx1.4,loto$lxtx2.4,loto$lxtx3.4,loto$lxtx4.4,loto$lxtx5.4,loto$lxtx6.4)
lxtx5 = c(loto$lxtx1.5,loto$lxtx2.5,loto$lxtx3.5,loto$lxtx4.5,loto$lxtx5.5,loto$lxtx6.5)
lxtx6 = c(loto$lxtx1.6,loto$lxtx2.6,loto$lxtx3.6,loto$lxtx4.6,loto$lxtx5.6,loto$lxtx6.6)
lxtx7 = c(loto$lxtx1.7,loto$lxtx2.7,loto$lxtx3.7,loto$lxtx4.7,loto$lxtx5.7,loto$lxtx6.7)
lxtx8 = c(loto$lxtx1.8,loto$lxtx2.8,loto$lxtx3.8,loto$lxtx4.8,loto$lxtx5.8,loto$lxtx6.8)
lxtx9 = c(loto$lxtx1.9,loto$lxtx2.9,loto$lxtx3.9,loto$lxtx4.9,loto$lxtx5.9,loto$lxtx6.9)
lxtx10 = c(loto$lxtx1.10,loto$lxtx2.10,loto$lxtx3.10,loto$lxtx4.10,loto$lxtx5.10,loto$lxtx6.10)
lxtx = c(lxtx,lxtx1,lxtx2,lxtx3,lxtx4,lxtx5,lxtx6,lxtx7,lxtx8,lxtx9,lxtx10)

lxtx.norm = c((loto$lxtx1/max(data$lxtx1)),(loto$lxtx2/max(data$lxtx2)),(loto$lxtx3/max(data$lxtx3)),(loto$lxtx4/max(data$lxtx4)),(loto$lxtx5/max(data$lxtx5)),(loto$lxtx6/max(data$lxtx6)))
lxtx.norm1 = c((loto$lxtx1.1/max(data$lxtx1.1)),(loto$lxtx2.1/max(data$lxtx2.1)),(loto$lxtx3.1/max(data$lxtx3.1)),(loto$lxtx4.1/max(data$lxtx4.1)),(loto$lxtx5.1/max(data$lxtx5.1)),(loto$lxtx6.1/max(data$lxtx6.1)))
lxtx.norm2 = c((loto$lxtx1.2/max(data$lxtx1.2)),(loto$lxtx2.2/max(data$lxtx2.2)),(loto$lxtx3.2/max(data$lxtx3.2)),(loto$lxtx4.2/max(data$lxtx4.2)),(loto$lxtx5.2/max(data$lxtx5.2)),(loto$lxtx6.2/max(data$lxtx6.2)))
lxtx.norm3 = c((loto$lxtx1.3/max(data$lxtx1.3)),(loto$lxtx2.3/max(data$lxtx2.3)),(loto$lxtx3.3/max(data$lxtx3.3)),(loto$lxtx4.3/max(data$lxtx4.3)),(loto$lxtx5.3/max(data$lxtx5.3)),(loto$lxtx6.3/max(data$lxtx6.3)))
lxtx.norm4 = c((loto$lxtx1.4/max(data$lxtx1.4)),(loto$lxtx2.4/max(data$lxtx2.4)),(loto$lxtx3.4/max(data$lxtx3.4)),(loto$lxtx4.4/max(data$lxtx4.4)),(loto$lxtx5.4/max(data$lxtx5.4)),(loto$lxtx6.4/max(data$lxtx6.4)))
lxtx.norm5 = c((loto$lxtx1.5/max(data$lxtx1.5)),(loto$lxtx2.5/max(data$lxtx2.5)),(loto$lxtx3.5/max(data$lxtx3.5)),(loto$lxtx4.5/max(data$lxtx4.5)),(loto$lxtx5.5/max(data$lxtx5.5)),(loto$lxtx6.5/max(data$lxtx6.5)))
lxtx.norm6 = c((loto$lxtx1.6/max(data$lxtx1.6)),(loto$lxtx2.6/max(data$lxtx2.6)),(loto$lxtx3.6/max(data$lxtx3.6)),(loto$lxtx4.6/max(data$lxtx4.6)),(loto$lxtx5.6/max(data$lxtx5.6)),(loto$lxtx6.6/max(data$lxtx6.6)))
lxtx.norm7 = c((loto$lxtx1.7/max(data$lxtx1.7)),(loto$lxtx2.7/max(data$lxtx2.7)),(loto$lxtx3.7/max(data$lxtx3.7)),(loto$lxtx4.7/max(data$lxtx4.7)),(loto$lxtx5.7/max(data$lxtx5.7)),(loto$lxtx6.7/max(data$lxtx6.7)))
lxtx.norm8 = c((loto$lxtx1.8/max(data$lxtx1.8)),(loto$lxtx2.8/max(data$lxtx2.8)),(loto$lxtx3.8/max(data$lxtx3.8)),(loto$lxtx4.8/max(data$lxtx4.8)),(loto$lxtx5.8/max(data$lxtx5.8)),(loto$lxtx6.8/max(data$lxtx6.8)))
lxtx.norm9 = c((loto$lxtx1.9/max(data$lxtx1.9)),(loto$lxtx2.9/max(data$lxtx2.9)),(loto$lxtx3.9/max(data$lxtx3.9)),(loto$lxtx4.9/max(data$lxtx4.9)),(loto$lxtx5.9/max(data$lxtx5.9)),(loto$lxtx6.9/max(data$lxtx6.9)))
lxtx.norm10 = c((loto$lxtx1.10/max(data$lxtx1.10)),(loto$lxtx2.10/max(data$lxtx2.10)),(loto$lxtx3.10/max(data$lxtx3.10)),(loto$lxtx4.10/max(data$lxtx4.10)),(loto$lxtx5.10/max(data$lxtx5.10)),(loto$lxtx6.10/max(data$lxtx6.10)))
lxtx.norm = c(lxtx.norm,lxtx.norm1,lxtx.norm2,lxtx.norm3,lxtx.norm4,lxtx.norm5,lxtx.norm6,lxtx.norm7,lxtx.norm8,lxtx.norm9,lxtx.norm10)

lxtx = lxtx.norm*dose.test

exp.lin = function(x,y) {91.5*(1-exp(-x/355))+0.015*x-y}
exp.sat = function(x,y) {123*(1-exp(-x/510))-y}
lxtx = as.data.frame(lxtx)
De.exp.lin = 1:dim(lxtx)[1]; De.exp.sat = 1:dim(lxtx)[1]
for (i in 1:dim(lxtx)[1]) {
  De.exp.lin[i] = try(uniroot(exp.lin, y=lxtx[i,1], lower = 0, upper = 5000)$root)
  De.exp.sat[i] = try(uniroot(exp.sat, y=lxtx[i,1], lower = 0, upper = 5000)$root)
}
De.res = data.frame(exp.lin = as.numeric(De.exp.lin),exp.sat = as.numeric(De.exp.sat))
