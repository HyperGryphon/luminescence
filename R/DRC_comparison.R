rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Resultados")
#cargar los datos
sample = "L0536"
data.gy = read.table(file = paste(sample,"_fdp.txt", sep = ""), header = T, fill = T, sep = "\t")

#señal = a * (1 - exp(Dose/b)) -> Kars et al (2008)
#a = Lx/Tx a 100% de saturación
#b = Dosis de saturación (2*b = 86%)

#Dosis (eje x)
D = c(1:800)

aliquot = 12
data.gy = data.gy[aliquot,]

#extraer De
de50 = na.omit(data.gy$de50)
de290 = na.omit(data.gy$de290)

#extraer parametro a
a50 = na.omit(data.gy$a50)
a290 = na.omit(data.gy$a290)

#extraer dosis de saturación
b50 = na.omit(data.gy$d050)
b290 = na.omit(data.gy$d0290)

#extraer valor c
#c50 = na.omit(data.gy$c50)
#c290 = na.omit(data.gy$c290)

#DRC
IRSL50 = a50*(1-exp(-D/b50))
IRSL290 = a290*(1-exp(-D/b290))

#plotear DRCs
matplot(D, cbind(IRSL50, IRSL290),
        type = "l",
        lty = 1,
        lwd = 2,
        xlab = "Dose (Gy)",
        ylab = "Corrected IRSL (Lx/Tx)",
        col = c("black", "red"),
        main = paste("Sample ", sample, " Aliquot ", aliquot),
        xaxs = "i", yaxs = "i", 
        xlim = c(0, max(D)), ylim = c(0, max(IRSL290)+1))

segments(x0 = 0, y0 = a50*(1-exp(-de50/b50)),
         x1 = data.gy$de50, y1 = a50*(1-exp(-de50/b50)),
         lty = 2, col = "black")
segments(x0 = data.gy$de50, y0 = a50*(1-exp(-de50/b50)),
         x1 = data.gy$de50, y1 = 0,
         lty = 2, col = "black")

points(a50*(1-exp(-de50/b50)), col = "black", pch = 16)

segments(x0 = 0, y0 = a290*(1-exp(-de290/b290)),
         x1 = data.gy$de290, y1 = a290*(1-exp(-de290/b290)),
         lty = 2, col = "red")
segments(x0 = data.gy$de290, y0 = a290*(1-exp(-de290/b290)),
         x1 = data.gy$de290, y1 = 0,
         lty = 2, col = "red")

points(a290*(1-exp(-de290/b290)), col = "red", pch = 16)

#leyenda
text(410, y = max(IRSL290)+0.6, (bquote("pIRIR"[290]~.(paste0("= ", de290, " Gy")))), 
     pch = 16, col = "red", cex = 1.2)
text(400, y = max(IRSL290), (bquote("pIRIR"[50]~.(paste0("= ", de50, " Gy")))), 
     pch = 16, col = "black", cex = 1.2)
