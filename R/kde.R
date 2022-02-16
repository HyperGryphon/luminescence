library(Luminescence)
library(readxl)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documentos/proyectos/Colombia/")
#cargar los datos
name = "L1206"
sample = read_excel("analyst qtz colombia.xlsx", sheet = name, col_names = T)
#sample = read.table("16211-3_HF2x_130312-TEMP165.asc", header = T, sep = ",")

De = as.numeric(sample$ED)#*0.088
De.Error = as.numeric(sample$ED_Err)#*0.088
d0 = as.numeric(sample$Param2)
recy = as.numeric(sample$RR1)
recu = as.numeric(sample$Recup1)
#ir.ratio = sample$`irsl/blsl %`
#ir.dep.ratio = sample$`ir dep ratio`
df1 = data.frame(De, De.Error, d0, recy, recu)
De.data = subset(df1, recy<=1.1 & recy>=0.9 & recu<=5)
De.data = data.frame(de=na.omit(De.data$De), de.err=na.omit(De.data$De.Error))
De = De.data$de; De.Error = De.data$de.err

#central age model
cam = calc_CentralDose(De.data,plot = F, log = T)
#minimum age model
mam = calc_MinDose(De.data,log = T,sigmab = 0.2,par = 3,bootstrap = F,plot = F)

mamres = get_RLum(mam, "summary")
camres = get_RLum(cam, "summary")

#pdf(paste0("kde_",name,".pdf"),height = 6, width = 7)
par(mar=c(5,5,1.5,1.5))
plot(density(De), xlab = "Dose (Gy)", ylab = "Density",
     cex.lab = 1.5, cex.axis = 1.5,
     #ylim = c(0,max(density(De)$y)+max(density(De)$y)*0.2),
     xlim = c(min(De)-min(De)*0.1, max(De)+max(De)*0.1), lwd = 2,
     main = paste0("Sample ",name, " | ",
                  paste0("OD = ",round(camres$rel_OD,0),"%"), " | ",
                  paste0("(n = ", nrow(De.data), "/", nrow(df1), ")", sep = "")))
par(new = T)
plot(y = 1:length(De), x = De[order(De, decreasing = F)], ylab = "", yaxt = "n", pch = 16,
     xlim = c(min(De)-min(De)*0.1, max(De)+max(De)*0.1), xlab = "", xaxt = "n",
     col = "black")
#añadir stds al kde plot
arrows(y0=1:length(De),y1=1:length(De), angle = 90, length = 0.05, code = 3,
       x0=De[order(De, decreasing = F)]-De.Error[order(De, decreasing = F)],
       x1=De[order(De, decreasing = F)]+De.Error[order(De, decreasing = F)])
#añadir lineas al kde plot
#abline(v = mamres$de,col = "black", lwd = 2)
abline(v = camres$de,col = "red", lwd = 2, lty = 2)
legend("bottomright",legend = c(paste("CAM: ", paste0(round(camres$de, 2), " \U00B1 ",
                                                round(camres$de_err, 2), " Gy"))),
       col = c("red"),lwd = 2, lty = 2, bty = "n", cex = 1.5)
#dev.off()