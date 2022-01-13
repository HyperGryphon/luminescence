rm(list = ls())
graphics.off()

library(Luminescence); library(xlsx)
#chose directory
setwd("C:/Users/iande/Documents/proyectos/Colombia/sec_bin/")
#chose sample
sample = "Sens_L1199-1200-1201-1204_R1_200929.binx"

#Select only "OSL" signals
CW = read_BIN2R(paste(sample, sep = ""))
n.aliquots = length(unique(CW@METADATA$POSITION))
tl = subset(CW, CW@METADATA$RUN == 10)
bg = subset(CW, CW@METADATA$RUN == 11)

#Estimate doses
dose.rate = data.frame(CW@METADATA$IRR_DOSERATE); dose.time = data.frame(CW@METADATA$IRR_TIME)
tl = data.frame(tl@DATA)
bg = data.frame(bg@DATA)
doses = data.frame(dose.rate*dose.time)
colnames(doses) = c("Dose (Gy)")

#select channels of integration and background
t = seq(1.25,500,1.25)
tlint = (80/1.25):(120/1.25); tltot = 1:400

########################################################################
################ just press ctrl+A and enter to run ####################
########################################################################

#sensitivity calculation 
tl.s = 1:length(tl)
tl.total = 1:length(tl); tl.sens = 1:length(tl)
for (i in 1:length(tl)) {
  
  tl.s[i] = sum(tl[,i][tlint])-sum(bg[,i][tlint])
  tl.total[i] = sum(tl[,i][tltot])
  tl.sens[i] = tl.s[i]/tl.total[i]*100
}


#condense and save
table = data.frame(tl.sens)
tl.sens
#colnames(table) = c("%BOSLf", "OSL (cts/s)")
#write.xlsx(table, file = paste0("sensitivity_Colombia.xlsx"), sheetName = sample, append = T)
