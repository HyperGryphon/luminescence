rm(list = ls())
setwd("C:/Users/iande/Documents/proyectos/Atacama - Tesis/sec_bin/")
#cargar los datos
sample = "L0530"
data.gy = read.table(file = paste(sample,".txt", sep = ""), header = T, fill = T, sep = "\t")

#señal = a * (1 - exp(Dose/b)) -> Kars et al (2008)
#a = Lx/Tx a 100% de saturación
#b = Dosis de saturación (2*b = 86%)

#Dosis (eje x)
D = c(1:3000)

data.gy = data.gy[1,]

#extraer parametro a
a50 = na.omit(data.gy$a50)
a290 = na.omit(data.gy$a290)

#la dosis de 290 parece ser 3x la de 50, por eso 1 +0.33, una tercera parte
a50mod = (1.33)*na.omit(data.gy$a50)

#extraer dosis de saturación (Do)
b50 = na.omit(((data.gy$d050)*2)/0.86)
b290 = na.omit(((data.gy$d0290)*2)/0.86)
b50mod = (1/1.33)*na.omit(((data.gy$d050)*2)/0.86)

#extraer valor c
#c50 = na.omit(data.gy$c50)

#DRC
IRSL50 = a50*(1-exp(-D/b50))
IRSL290 = a290*(1-exp(-D/b290))
IRSL_mod = a50mod*(1-exp(-D/b50mod))

#DRC modelada
s = 3e15
t = 300000
p = 3.6e-6
irsl_mod = IRSL50*exp(-p*(log10(1.8*s*t))^3)

#calcular dosis equivalente corregida
lxtx = (a50mod*(1-exp(-data.gy$de50/b50mod)))
De_mod = b50mod/log10(lxtx)

#plotear DRCs
matplot(D, cbind(IRSL50, IRSL290, IRSL_mod, irsl_mod),
        type = "l",
        lty = 2:3,
        lwd = 0.5,
        xlab = "Dose (Gy)",
        ylab = "IRSL (Lx/Tx)",
        xlim = c(0, max(D)),
        ylim = c(0, max(IRSL_mod+1)),
        xaxs = "i", yaxs = "i",
        col = c("red", "cornflowerblue", "black", "gray"),
        main = paste("Sample ", sample))

abline(a = data.gy$de50,
       b = 0,
       v = data.gy$de50,
       h = a50*(1-exp(-data.gy$de50/b50)),
       col = "red")
abline(a = De_mod,
       b = 0,
       v = De_mod,
       h = a50mod*(1-exp(-De_mod/b50mod)),
       col = "black")

#leyenda
legend("bottomright",
       legend = c(paste("De50 = ", data.gy$de50, "Gy"),
                  paste("De290 = ", data.gy$de290, "Gy"),
                  paste("De_mod = ", signif(De_mod, digits = 6),  "Gy")),
                  text.col = c("red", "cornflowerblue" ,"black"))