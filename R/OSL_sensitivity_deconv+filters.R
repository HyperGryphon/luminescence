rm(list = ls())
graphics.off()

library(Luminescence); library(xlsx)
#chose directory
setwd("G:/Mi unidad/Parnaiba/binfiles/")
#chose sample
sample = "Sens_Amazon_ParnaibaSeq_STM60-61-XA80-78_R2_201106.binx"
#aliquot = 1
#run = 10

#Select only "OSL" signals
data = read_BIN2R(paste(sample, sep = ""))
n.aliquots = length(unique(data@METADATA$POSITION))
CW = subset(data, data@METADATA$LTYPE == "OSL" & data@METADATA$RUN == 5)# & data@METADATA$AN_TEMP==220 & data@METADATA$RUN == 6)
#Estimate doses
dose.rate = data.frame(data@METADATA$IRR_DOSERATE); dose.time = data.frame(data@METADATA$IRR_TIME)
CW = data.frame(CW@DATA)
doses = data.frame(dose.rate*dose.time)
colnames(doses) = c("Dose (Gy)")

#select channels of integration and background
t = seq(0.25,100,0.25)
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
    sens[i] = 0
  } 
  else  if (osl[i] < (bg.osl[i]+3*sd.bg.osl[i])) {
    sens[i] = 0
  } 
  #else  if (osl[i] > (bg.osl[i]+3*sd.bg.osl[i]) & osl[i] < (2*bg.osl[i]+3*sd.bg.osl[i])) {
  #sens[i] = print("low")
  #}
  else {
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

#calculate contribution###########################################
#par(new = T, mfrow = c(2,2))
fast.prop = 1:length(CW); med.prop = 1:length(CW); slow.prop = 1:length(CW); slow.2.prop = 1:length(CW)
fast = 1:length(CW); med = 1:length(CW); slow = 1:length(CW); slow.2 = 1:length(CW)

for (i in 1:length(CW)) {
  fit = fit_CWCurve(data.frame(t,CW[,i]), n.components.max = components, fit.method = "LM",
                    fit.trace = F, fit.failure_threshold = T, 
                    LED.power = 40, LED.wavelength = 470,
                    #log = "x",
                    plot = T)
  if (ncol(fit$component.contribution.matrix[[1]]) == 15) {
    fast.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    med.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])/
                     (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    slow.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    slow.2.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])/
                        (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                           sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                           sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                           sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    fast[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c3"])
    slow.2[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c4"])
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 12) {
    fast.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])))*100
    med.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])/
                     (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])))*100
    slow.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])))*100
    slow.2.prop[i] = print(NA)
    fast[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c3"])
    slow.2[i] = data.frame(print(NA))
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 9) {
    fast.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])))*100
    med.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])/
                     (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])))*100
    slow.prop[i] = print(NA)
    slow.2.prop[i] = print(NA)
    fast[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] = data.frame(print(NA))
    slow.2[i] = data.frame(print(NA))
    
  }
  if(ncol(fit$component.contribution.matrix[[1]]) == 6) {
    fast.prop[i] = (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])))*100
    med.prop[i] = print(NA)
    slow.prop[i] = print(NA)
    slow.2.prop[i] = print(NA)
    fast[i] = data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] = data.frame(print(NA))
    slow[i] = data.frame(print(NA))
    slow.2[i] = data.frame(print(NA))
  }
}

#condense data
if (components == 3) {
  fast.rec = (fast.prop/(fast.prop+med.prop+slow.prop))*100
  med.rec = (med.prop/(fast.prop+med.prop+slow.prop))*100
  slow.rec = (slow.prop/(fast.prop+med.prop+slow.prop))*100
  fast.prop = fast.rec; med.prop = med.rec; slow.prop = slow.rec
  #fast.ratio = cbind(fast.ratio, fast.ratio.se)
  comp.prop = data.frame(sens, fast.prop, med.prop, slow.prop)
  colnames(comp.prop) = c("%BOSLf","fast (% of BOSLf)", "medium (% of BOSLf)", "slow (% of BOSLf)")
  osl.comp = data.frame(osl, osl*fast.prop/100, osl*med.prop/100, osl*slow.prop/100)
  colnames(osl.comp) = c("OSL (cts/1s)","fast (cts/1s)", "med (cts/1s)", "slow (cts/1s)")
  comp = data.frame(mean(fast.prop), mean(na.omit(med.prop)), mean(na.omit(slow.prop)))
  comp.sd = data.frame(sd(fast.prop), sd(na.omit(med.prop)), sd(na.omit(slow.prop)))
  comp.se = data.frame(sd(fast.prop)/(sqrt(length(fast.prop))), 
                       sd(na.omit(med.prop))/(sqrt(length(med.prop))), 
                       sd(na.omit(slow.prop))/(sqrt(length(slow.prop))))
  table = cbind(comp.prop, osl.comp)
}

#plot results
#plot(1:length(CW),fast.ratio[,1])
par(mar = c(4,4,4,6), xpd = T)
matplot(cbind(fast.prop, med.prop, slow.prop), pch = 15:17, col = 2:4,
        xlab = "aliquot", ylab = "% proportion")
legend("topright", inset = c(-0.15, 0), pch = 15:17, col = 2:4,
       legend = c("Fast", "Medium", "Slow"))

#create excel file
write.xlsx(table, file = paste0(sample, ".xlsx"), append = T)

