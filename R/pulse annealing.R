#rm(list = ls())
#graphics.off()

library(Luminescence); library(xlsx)
#chose directory
setwd("C:/Users/iande/Documents/proyectos/Colombia/sec_bin/")
#chose sample
sample = "Pulse-Anneal_Sens_L1199-1200-1201-1204_R1_200929.binx"

#Select only "OSL" signals
CW = read_BIN2R(paste(sample, sep = ""))
n.aliquots = length(unique(CW@METADATA$POSITION))
CW = subset(CW, CW@METADATA$POSITION == 15)

#Estimate doses
dose.rate = data.frame(CW@METADATA$IRR_DOSERATE); dose.time = data.frame(CW@METADATA$IRR_TIME)
temps = data.frame(CW@METADATA$AN_TEMP)
CW = data.frame(CW@DATA)
doses = data.frame(dose.rate*dose.time)
colnames(doses) = c("Dose (Gy)")

#select channels of integration and background
t = seq(0.1,40,0.1)
ch = 8; bg1 = 301; bg2 = 400

########################################################################
################ just press ctrl+A and enter to run ####################
########################################################################

#sensitivity calculation 
osl.s = 1:length(CW); bg.osl = 1:length(CW)
osl.total = 1:length(CW); bg.total = 1:length(CW)
sens.osl = 1:length(CW); osl = 1:length(CW)
sens = 1:length(CW); sd.bg.osl = 1:length(CW)
for (i in 1:length(CW)) {
  
  osl.s[i] = sum(CW[,i][1:ch])
  osl.total[i] = sum(CW[,i][1:bg2])
  bg.osl[i] = mean(CW[,i][bg1:bg2])*length(CW[,i][1:ch])
  bg.total[i] = mean(CW[,i][bg1:bg2])*length(CW[,i][1:bg2])
  sd.bg.osl[i] = sd(CW[,i][bg1:bg2]) 
  # OSL in cts/1s
  osl[i] = osl.s[i]-bg.osl[i]
  # %BOSLF
  sens.osl[i] = osl[i]/(osl.total[i]-bg.total[i])*100
}

#condense and save
table = data.frame(sens.osl,osl, doses, temps)
colnames(table) = c("%BOSLf", "OSL (cts/s)", "Dose (Gy)", "Anneal_Temp (ºC)")
even = seq(2,dim(table)[1],2)
odd = seq(1,dim(table)[1],2)
signal = table[odd,]
test = table[even,]
lxtx = signal[,2]/test[,2]
lxtx.data = data.frame(lxtx, signal$`Dose (Gy)`, signal$`Anneal_Temp (ºC)`)
lxtx.data

plot(lxtx.data[2:max(nrow(lxtx.data)),3], lxtx.data[2:max(nrow(lxtx.data)),1], 
     xlab = "", ylab = "Lx/Tx", pch = 16, xaxt = "n", cex = 1.5, cex.axis = 1.2, cex.lab = 1.5)
axis(1, at = seq(180,340,20), cex.axis = 1.2)
mtext(side = 1, "Temperature (ºC)", line = 2.2, cex = 1.5)

test = subset(table, table$`Anneal_Temp (ºC)`==160)
test = test[-1,]
plot(test$`%BOSLf`, ylim = c(0,100), xlab = "SAR cycle", ylab = expression("%BOSL"[F]))
axis(3, at = seq(180,340,20), cex.axis = 1.2)
mtext(side = 3, "Preheat temperature (ºC)", line = 2.2, cex = 1.5)