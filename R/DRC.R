rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Resultados")
#cargar los datos
sample = "L0478_fdp"
data.gy = read.table(file = paste(sample,".txt", sep = ""), header = T, fill = T, sep = "\t")

#señal = a * (1 - exp(Dose/b)) -> Kars et al (2008)
#a = Lx/Tx a 100% de saturación
#b = Dosis de saturación (2*b = 86%)

#extraer parametro a
a = na.omit(data.gy$a290)

#extraer dosis de saturación
b = na.omit(((data.gy$d0290)*2)/0.86)

data = data.frame(a,b)

#DRC
lxtx = sapply(1:3000, function(D) data$a*(1-exp(-D/data$b)))

#plotear DRCs
matplot(t(lxtx),
        type = "l",
        lty = 1,
        lwd = 0.1,
        xlab = "Dose (Gy)", 
        ylab = "IRSL (Li/Ti)",
        col = "#808080",
        main = paste("Sample ", sample),
        xaxs = "i", yaxs = "i", xlim = c(0, 3000), ylim = c(0, max(lxtx)+0.5))

#abline(v = mean(na.omit(data.gy$de290)),col = "#cd2828")

#leyenda
#legend("bottomright", legend = c(paste("Aliquot ", 1:length(a))), ncol = 3, text.col = "#000000")
legend("bottomright",
       legend = c(paste("De =", round(mean(na.omit(data.gy$de290)), 0), "Gy"),
                  paste("D0 =", round(mean(na.omit(b)), 0), "Gy")),
       text.col = c("#000000"))