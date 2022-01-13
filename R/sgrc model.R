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

#matplot(data$dose,cbind(data$lxtx1,data$lxtx2,data$lxtx3,data$lxtx4,data$lxtx5,data$lxtx6), 
#        ylim = c(0,5),pch = 16, col = rainbow(11))

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

lxtx = lxtx.norm*dose.test
#lxtx = lxtx.norm

R2 = 0.5
sgrc1 = 100; sgrc2 = 1000; sgrc3 = 0.0005
#while (R2 < 0.95) {
sgrc = nls(lxtx~a*(1-exp(-dose/b))+c*dose, start = list(a = sgrc1, b = sgrc2, c = sgrc3))
sgrc1 = coef(sgrc)[1]; sgrc2 = coef(sgrc)[2]; sgrc3 = coef(sgrc)[3]
sgrc1;sgrc2;sgrc3
R2 = 1-((sum(residuals(sgrc)^2))/(sum((na.omit(lxtx) - mean(na.omit(lxtx)))^2)))

residuals = data.frame(res = as.numeric(residuals(sgrc)), samples)
residuals %>% 
  group_by(samples) %>% 
  summarize(avg=mean(res)) -> mean.residuals
residuals %>% 
  group_by(samples) %>% 
  summarize(sd=sd(res)) ->sd.residuals

residuals.mean = c(rep(mean.residuals[1,2],6),rep(mean.residuals[2,2],6),rep(mean.residuals[3,2],6),rep(mean.residuals[4,2],6),rep(mean.residuals[5,2],6),rep(mean.residuals[6,2],6),rep(mean.residuals[7,2],6),rep(mean.residuals[8,2],6),rep(mean.residuals[9,2],6),rep(mean.residuals[10,2],6),rep(mean.residuals[11,2],6))
mean.residuals = as.numeric(residuals.mean)

#lxtx = lxtx-mean.residuals

#sgrc = nls(lxtx~a*(1-exp(-dose/b))+c*dose, start = list(a = sgrc1, b = sgrc2, c = sgrc3),
#                control = list(maxiter = 100), trace = T)
#sgrc1 = coef(sgrc)[1]; sgrc2 = coef(sgrc)[2]; sgrc3 = coef(sgrc)[3]
#sgrc1;sgrc2;sgrc3
#R2 = 1-((sum(residuals(sgrc)^2))/(sum((na.omit(lxtx) - mean(na.omit(lxtx)))^2)))
#}
sgrc1;sgrc2;sgrc3
R2

#sgrc.norm = nls(lxtx.norm~a*(1-exp(-dose/b)), start = list(a = 200, b = 1000))
#sgrc3 = coef(sgrc.norm)[1]; sgrc4 = coef(sgrc.norm)[2]; sgrc5 = coef(sgrc.norm)[3]
#sgrc3;sgrc4
#R2.norm = 1-((sum(residuals(sgrc.norm)^2))/(sum((na.omit(lxtx.norm) - mean(na.omit(lxtx.norm)))^2)))

#data = data.frame(dose,lxtx)
#data.norm = data.frame(dose,lxtx.norm)

#pdf("sgrc.pdf", width = 8, height = 6)
xlim = c(0,2500); ylim = c(0,max(na.omit(lxtx))+0.2*max(na.omit(lxtx)))
par(mar = c(5,5,1,1))
plot(0:2500, sgrc1*(1-exp(-(0:2500)/sgrc2))+sgrc3*(0:2500), type = "l", col = "red", xlim = xlim, ylim = ylim,
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", lwd = 2, lty = 2)
par(new = T, mar = c(5,5,1,1))
plot(x = dose, y = lxtx, xlab = "Dose (Gy)", ylab = "LxTx", cex = 1.1, xlim = xlim, ylim = ylim,
     pch = 16, cex.lab = 1.1, cex.axis = 1.1)
legend("bottomright", col = c("red"), lwd = 3, lty = 2, cex = 1.5,
       legend = c(expression(10.9*(1-exp(-x/444))+0.0014*x~"|"~R^2~" = 0.99")))
#dev.off()


