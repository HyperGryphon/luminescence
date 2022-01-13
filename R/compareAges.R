library(Hmisc)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Downloads/Pecube/STA01")
data = read.csv("CompareAGE.csv", header = T, fill = T, sep = ",")
df = data.frame(rev(data))
name = df[,1]
osl_lbg = df[,2]; osl_ebg = df[,4]
osl_lbg_err = df[,3]; osl_ebg_err = df[,5]

par(mar = c(4.5,4.5,2,1.5))
errbar(name, osl_lbg, osl_lbg+osl_lbg_err, osl_lbg-osl_lbg_err,
       ylab = "Age (ka)", xlab = "Sample", cex = 1.5, cex.axis = 1.5, cex.lab = 1.5)
