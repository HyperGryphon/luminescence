library(Luminescence); library(xlsx)
rm(list = ls())
graphics.off()
#chose directory
setwd("C:/Users/iande/Documents/proyectos/Purus/sec_bin/")
#chose sample
sample1 = "Dating_L1416_R1_200106.binx"
sample = read_BIN2R(sample1)

#integration time in channels
oslint = 1:10; oslbg = 301:400; osltot = 1:400
#chose run number depending on the position of the OSL sens sequence
osl = subset(sample, sample@METADATA$RUN == 6)
osl = data.frame(osl@DATA)

signal.osl = 1:length(osl)

for (i in 1:length(osl)) {
  
  signal.osl[i] = (sum(osl[,i][oslint])-sum(osl[,i][oslbg]*0.1))/(sum(osl[,i][osltot])-sum(osl[,i][oslbg]*4))*100
}

osl = cbind(signal.osl)
write.xlsx(osl, file = "osl_sens.xlsx", sheetName = paste(sample1))
boxplot(osl)
matplot(cbind(osl), pch = 15, col = 2, xlab = "aliquot", ylab = "%BOSL")

sens = mean(signal.osl)
sens.sd = sd(signal.osl)
sens.se = sd(signal.osl)/sqrt(length(osl))
cbind(sens, sens.sd, sens.se)