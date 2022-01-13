rm(list = ls())
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados")
#cargar los datos
sample = "L0478_DoseRec"
data.gy = read.table(file = paste(sample,".txt", sep = ""), header = T, fill = T, sep = "\t")

#señal = a * (1 - exp(Dose/b)) -> Kars et al (2008)
#a = Lx/Tx a 100% de saturación
#b = Dosis de saturación (2*b = 86%)

#Dosis (eje x)
D = c(1:1500)

data.gy = data.gy[3,]

#extraer parametro a
t1a225 = na.omit(data.gy$t1a225)
t2a290 = na.omit(data.gy$t2a290)
t3a290 = na.omit(data.gy$t3a290)

#extraer dosis de saturación
t1b225 = na.omit(((data.gy$t1d0225)*2)/0.86)
t2b290 = na.omit(((data.gy$t2d0290)*2)/0.86)
t3b290 = na.omit(((data.gy$t3d0290)*2)/0.86)

#extraer valor c
#c50 = na.omit(data.gy$c50)
#c290 = na.omit(data.gy$c290)

#DRC
IRSL225 = t1a225*(1-exp(-D/t1b225))
IRSL290 = t2a290*(1-exp(-D/t2b290))
#la de 290ºC con bleaching previo a 325ºC
IRSL290b = t3a290*(1-exp(-D/t3b290))

#plotear DRCs
matplot(D, cbind(IRSL225, IRSL290, IRSL290b),
        type = "l",
        lty = c(1,2,4),
        lwd = 2,
        xlab = "Dose (Gy)",
        ylab = "IRSL (Li/Ti)",
        col = c("#000000", "#CD2828", "#387fda"),
        main = paste("Dose Recovery: ", sample),
        xaxs = "i", yaxs = "i", xlim = c(0, max(D)), ylim = c(0, max(IRSL225)+0.5))

abline(a = data.gy$t1de225,
       b = 0,
       v = data.gy$t1de225,
       h = t1a225*(1-exp(-data.gy$t1de225/t1b225)),
       col = "#000000",
       lty = 2)
abline(a = data.gy$t2de290,
       b = 0,
       v = data.gy$t2de290,
       h = t2a290*(1-exp(-data.gy$t2de290/t2b290)),
       col = "#CD2828",
       lty = 1)
abline(a = data.gy$t3de290,
       b = 0,
       v = data.gy$t3de290,
       h = t3a290*(1-exp(-data.gy$t3de290/t3b290)),
       col = "#387fda",
       lty = 1)

#leyenda
legend("bottomright",
       legend = c(paste("IRSL225 =", data.gy$t1de225, "Gy"), 
                  paste("IRSL290 =", data.gy$t2de290, "Gy"),
                  paste("IRSL290b =", data.gy$t3de290, "Gy")),
       text.col = c("#000000", "#CD2828", "#387fda"),
       cex = 0.8)