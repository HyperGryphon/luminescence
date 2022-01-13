rm(list = ls())
graphics.off()

library(Luminescence); library(xlsx)
#chose directory
setwd("C:/Users/iande/Documents/proyectos/Atacama/sec_bin/")
#chose sample
sample = "Dating_L0678_R1.binx"
#aliquot = 1
#run = 10

#Select only "OSL" signals
data = read_BIN2R(paste(sample, sep = ""))
n.aliquots = length(unique(data@METADATA$POSITION))
CW = subset(data, data@METADATA$LTYPE == "OSL")# & data@METADATA$AN_TEMP==220 & data@METADATA$RUN == 6)
#Estimate doses
dose.rate = data.frame(data@METADATA$IRR_DOSERATE); dose.time = data.frame(data@METADATA$IRR_TIME)
CW = data.frame(CW@DATA)
doses = data.frame(dose.rate*dose.time)
colnames(doses) = c("Dose (Gy)")

#select channels of integration and background
t = seq(0.1,40,0.1)
ch = 10; bg1 = 301; bg2 = 400

#how many components, max = 4
components = 3

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
  sd.bg.osl[i] = sd(CW[,i][bg1:bg2])
  bg.total[i] = mean(CW[,i][bg1:bg2])*length(CW[,i][1:bg2])
  # OSL in cts/1s
  osl[i] = (osl.s[i]-bg.osl[i]) #/(doses[i]*8.9) #cts/Gy·mg
  # %BOSLF
  sens.osl[i] = (osl.s[i]-bg.osl[i])/(osl.total[i]-bg.total[i])*100
 
  if (sens.osl[i]<0) {
    sens[i] = print("dim")
  } else  if (osl[i] < (bg.osl[i]+3*sd.bg.osl[i])) {
    sens[i] = print("dim")
  } else  if (osl[i] > (bg.osl[i]+3*sd.bg.osl[i]) & osl[i] < (2*bg.osl[i])) {
    sens[i] = print("low")
  } else {
  sens[i] = sens.osl[i]
  }
}

sens = unlist(sens)
sens.n = as.numeric(na.omit(sens))
mean(na.omit(sens.n)); sd(na.omit(sens.n))
dims = length(which(sens == "dim"))
lows = length(which(sens == "low"))
dims/length(CW)*100
lows/length(CW)*100

plot(sens.n, pch = 1, cex = 1.5)