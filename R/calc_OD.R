library(Luminescence)
library(readxl)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documentos/proyectos/Bremen Core/")
#cargar los datos
name = "218-1"
sample = read_excel("analyst_16218.xlsx", sheet = name, col_names = T)
#sample = read.table("16211-3_HF2x_130312-TEMP165.asc", header = T, sep = ",")

log = T

De = sample$ED*0.088
De.Error = sample$ED_Err*0.088
d0 = sample$Param2*0.088
recy = sample$RR1
recu = sample$Recup1
#ir.ratio = sample$`irsl/blsl %`
#ir.dep.ratio = sample$`ir dep ratio`
df1 = data.frame(De, De.Error, d0, recy, recu)
De.data = data.frame(na.omit(df1$De), na.omit(df1$De.Error))
De.data = subset(De.data, recy<=1.1 & recy>=0.9 & recu<=5)
colnames(De.data) = c("De","De.Error")

#central age model
cam = calc_CentralDose(De.data,
                       plot = F, log = log)

sigmab = 0

if (log) {
  yu = log(De.data$De)
  su = sqrt((De.data$De.Error/De.data$De)^2+sigmab^2)
} else {
  yu = De.data$De
  su = sqrt((De.data$De.Error)^2+sigmab^2)
}

sigma = 0.15 #starting value
wu = 1/(sigma^2+su^2)
delta = sum(wu*yu)/sum(wu)
#n = length(yu)

#iterate for OD
for (i in 1:200) {
  delta = sum(wu*yu)/sum(wu)
  sigma = sigma*sqrt(sum((wu^2)*(yu-delta)^2/sum(wu)))
  wu = 1/(sigma^2+su^2)
}

#results
dose = ifelse(log, exp(delta), delta)
dose.se = 1/sqrt(sum(wu))
od = ifelse(log, sigma*100, sigma/dose*100)
od.se = 1/sqrt(2*sigma^2*sum(wu^2))

#standard errors
if (log) {
  out.dose.se = dose.se*100
  out.od.se = od.se
} else {
  out.dose.se = dose.se/dose*100
  out.od.se = sqrt((dose.se/dose)^2 +(od.se/out.dose*100/out.od)^2)*out.od/100
  
}

dose.se = dose*dose.se
od.se = od.se*100
int.od = od; ext.od = 20
tot.od = sqrt(int.od^2+ext.od^2)

camres = data.frame(round(dose,2), round(dose.se,2), round(od,2), round(od.se,2), round(tot.od,2))
colnames(camres) = c("De", "De.se", "Int.OD", "Int.OD.se", "Tot.OD")
camres

#OD model Smedley et al. (2020)############################################################
png("ODvsOD.png", type = "cairo", height = 2000, width = 2200, res = 300)
par(mar = c(5,5,1.5,1.5))
plot(0:100, sqrt((0:100)^2+20^2), xlim = c(0,100), ylim = c(0,100), type = "l", lty = 1, lwd = 2,
     xlab = "Measured OD (%)", ylab = "Calculated OD (%)", xaxs = "i", yaxs = "i",
     cex.lab = 1.5, cex.axis = 1.5)
abline(0,1, lty = 2, lwd = 2)
legend("topleft", c("Calculated OD", "Measured OD"), lty = c(1,2), lwd = 2, cex = 1.5)
dev.off()
