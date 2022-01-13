rm(list = ls())
#graphics.off()

library(Luminescence); library(xlsx); library(tidyverse)
#chose directory
setwd("C:/Users/iande/Documentos/proyectos/Purus/binfiles/")
#chose sample
sample = "Dating_L1418_R2_200101.binx"
#aliquot = 1
#run = 10

#Select only "OSL" signals
CW = read_BIN2R(paste(sample, sep = ""))
n.aliquots = length(unique(CW@METADATA$POSITION))
CW = subset(CW, CW@METADATA$LTYPE == "OSL" & CW@METADATA$RUN == 3)
#Estimate doses
dose.rate = data.frame(CW@METADATA$IRR_DOSERATE); dose.time = data.frame(CW@METADATA$IRR_TIME)
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
osl.s <- 1:length(CW); bg.osl <- 1:length(CW)
osl.total <- 1:length(CW); bg.total <- 1:length(CW)
sens.osl <- 1:length(CW); osl.cts <- 1:length(CW)
sens <- 1:length(CW); sd.bg.osl <- 1:length(CW)
for (i in 1:length(CW)) {
  
  osl.s[i] <- sum(CW[,i][1:ch])
  osl.total[i] <- sum(CW[,i][1:bg2])
  bg.osl[i] <- mean(CW[,i][bg1:bg2])*length(CW[,i][1:ch])
  sd.bg.osl[i] <- sd(CW[,i][bg1:bg2])
  bg.total[i] <- mean(CW[,i][bg1:bg2])*length(CW[,i][1:bg2])
  # OSL in cts/1s
  osl.cts[i] <- (osl.s[i]-bg.osl[i]) #/(doses[i]*8.9) #cts/Gy·mg
  # BOSL1s
  sens.osl[i] <- (osl.s[i]-bg.osl[i])/(osl.total[i]-bg.total[i])*100
  
  if (is.na(sens.osl[i]) == TRUE) {
    sens.osl[i] <- 0
  }
  else if (sens.osl[i] < 0) {
    sens.osl[i] <- 0
  } 
  else if (sens.osl[i] > 100) {
    sens.osl[i] <- 100
  }
  else if (osl.cts[i] < (bg.osl[i]+3*sd.bg.osl[i])) {
    sens.osl[i] <- 0
  } 
  #else  if (osl[i] > (bg.osl[i]+3*sd.bg.osl[i]) & osl[i] < (2*bg.osl[i]+3*sd.bg.osl[i])) {
  #sens[i] <- print("low")
  #}
  else {
    sens.osl[i] <- sens.osl[i]
  }
}


#fast ratio (Durcan and Duller, 2011)
fast.ratio = 1:length(CW)
fast.ratio.se = 1:length(CW)
for (i in 1:length(CW)) {
  #fr = as.data.frame(
  #cbind(
  fr = calc_FastRatio(data.frame(t,CW[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                        stimulation.power = 40, wavelength = 470,
                        sigmaF = 2.32e-17,
                        sigmaM = 5.59e-18, plot = F)#))
  fast.ratio[i] = data.frame(fr$summary$fast.ratio)
  fast.ratio.se[i] = data.frame(fr$summary$fast.ratio.se)
}

#calculate contribution###########################################
#par(new = T, mfrow = c(2,2))
fast.prop = med.prop = slow.prop = slow.2.prop = 1:length(CW)
fast = med = slow = slow.2 = 1:length(CW)
cs1 = cs2 = cs3 = cs4 = matrix("NA",length(CW),1)

for (i in 1:length(CW)) {
  fit = fit_CWCurve(data.frame(t,CW[,i]), n.components.max = components, fit.method = "LM",
                    fit.trace = F, fit.failure_threshold = T, 
                    LED.power = 40, LED.wavelength = 470,
                    #log = "x",
                    plot = F)
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
    cs1[i] = c(fit$data$cs1)
    cs2[i] = c(fit$data$cs2)
    cs3[i] = c(fit$data$cs3)
    cs4[i] = c(fit$data$cs4)
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
    cs1[i] = c(fit$data$cs1)
    cs2[i] = c(fit$data$cs2)
    cs3[i] = c(fit$data$cs3)
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
    cs1[i] = c(fit$data$cs1)
    cs2[i] = c(fit$data$cs2)
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
    cs1[i] = c(fit$data$cs1)
  }
}

#condense data
if (components == 4) {
  fast.rec = (fast.prop/(fast.prop+med.prop+slow.prop))*100
  med.rec = (med.prop/(fast.prop+med.prop+slow.prop))*100
  slow.rec = (slow.prop/(fast.prop+med.prop+slow.prop))*100
  fast.prop = fast.rec; med.prop = med.rec; slow.prop = slow.rec
  fast.ratio = cbind(fast.ratio, fast.ratio.se)
  comp.prop = data.frame(sens.osl, fast.prop, med.prop, slow.prop)
  colnames(comp.prop) = c("%BOSL1s","fast (% of BOSLf)", "medium (% of BOSLm)", "slow (% of BOSLs)")
  osl.comp = data.frame(osl.cts, osl.cts*fast.prop/100, osl.cts*med.prop/100, osl.cts*slow.prop/100)
  colnames(osl.comp) = c("OSL (cts/1s)","fast (cts/1s)", "med (cts/1s)", "slow (cts/1s)")
  comp = data.frame(mean(fast.prop), mean(na.omit(med.prop)), mean(na.omit(slow.prop)))
  comp.sd = data.frame(sd(fast.prop), sd(na.omit(med.prop)), sd(na.omit(slow.prop)))
  comp.se = data.frame(sd(fast.prop)/(sqrt(length(fast.prop))), 
                       sd(na.omit(med.prop))/(sqrt(length(med.prop))), 
                     sd(na.omit(slow.prop))/(sqrt(length(slow.prop))))
  sar.cycle = data.frame(rep(c("natural", "test nat", "regen 1", "test 1", "regen 2", "test 2", "regen 3", "test 3", "regen 4", "test 4", 
                "recup regen", "recup test", "recycle regen", "recycle test", "post-IRSL regen", "post-IRSL test"), n.aliquots))
  colnames(sar.cycle) = c("SAR cycle")
  cs = data.frame(as.numeric(cs1),as.numeric(cs2),as.numeric(cs3),as.numeric(cs4))
  colnames(cs) = c("cs1 (cm^2)","cs2 (cm^2)","cs3 (cm^2)","cs3 (cm^2)")
  table = cbind(comp.prop, osl.comp, doses, sar.cycle,cs)

  }else{
  comp.prop = data.frame(sens.osl, fast.prop, med.prop, slow.prop)
  colnames(comp.prop) = c("%BOSL1s","fast (% of BOSLf)", "medium (% of BOSLm)", "slow (% of BOSLs)")
  osl.comp = data.frame(osl.cts, osl.cts*fast.prop/100, osl.cts*med.prop/100, osl.cts*slow.prop/100)
  colnames(osl.comp) = c("OSL (cts/1s)","fast (cts/1s)", "med (cts/1s)", "slow (cts/1s)")
  comp = data.frame(mean(fast.prop), mean(na.omit(med.prop)), mean(na.omit(slow.prop)))
  comp.sd = data.frame(sd(fast.prop), sd(na.omit(med.prop)), sd(na.omit(slow.prop)))
  comp.se = data.frame(sd(fast.prop)/(sqrt(length(fast.prop))), 
                       sd(na.omit(med.prop))/(sqrt(length(med.prop))), 
                       sd(na.omit(slow.prop))/(sqrt(length(slow.prop))))
  sar.cycle = data.frame(rep(c("natural", "test nat", "regen 1", "test 1", "regen 2", "test 2", "regen 3", "test 3", "regen 4", "test 4", 
                               "recup regen", "recup test", "recycle regen", "recycle test", "post-IRSL regen", "post-IRSL test"), n.aliquots))
  colnames(sar.cycle) = c("SAR cycle")
  cs = data.frame(as.numeric(cs1),as.numeric(cs2),as.numeric(cs3))
  colnames(cs) = c("cs1 (cm^2)","cs2 (cm^2)","cs3 (cm^2)")
  table = cbind(comp.prop, osl.comp, doses, cs)
  }

#create excel file
even = seq(2,dim(table)[1],2)
odd = seq(1,dim(table)[1],2)

test = table[even,]
regen = table[odd,]

#write.xlsx(test, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "test", append = T)
#write.xlsx(regen, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "regen", append = T)

#plot results
setwd("C:/Users/iande/Documentos/proyectos/Purus")
pdf(paste0("sample_", str_sub(sample, 8, 12), ".pdf"), width = 12, height = 10)
par(mar = c(4.5,4.5,2,1), mfrow = c(2,2))
plot(1:dim(regen)[1],regen$`%BOSL1s`, pch = 16, col = rainbow(12), cex = 2,
     xlab = "run", ylab = expression("%BOSL"["1s"]), ylim = c(0,100))
mtext(text = expression("Regen %BOSL"["1s"]), side = 3, line = 0)
plot(1:dim(test)[1],test$`%BOSL1s`, pch = 16, col = rainbow(12), cex = 2,
     xlab = "run", ylab = expression("%BOSL"["1s"]), ylim = c(0,100))
mtext(text = expression("Test %BOSL"["1s"]), side = 3, line = 0)
plot(1:dim(regen)[1],regen$`fast (% of BOSLf)`, pch = 16, col = rainbow(12), cex = 2,
     xlab = "run", ylab = expression("% fast OSL comp"), ylim = c(0,100))
mtext(text = "Regen % fast OSL comp", side = 3, line = 0)
plot(1:dim(test)[1],test$`fast (% of BOSLf)`, pch = 16, col = rainbow(12), cex = 2,
     xlab = "run", ylab = expression("% fast OSL comp"), ylim = c(0,100))
mtext(text = "Test % fast OSL comp", side = 3, line = 0)
dev.off()

regen.norecup = subset(regen, regen$`SAR cycle`!="recup regen")
bosl1s.regen = mean(na.omit(regen.norecup[,1]))
bosl1s.regen.sd = sd(na.omit(regen.norecup[,1]))
bosl1s.test = mean(na.omit(test[,1]))
bosl1s.test.sd = sd(na.omit(test[,1]))

fast.regen = mean(na.omit(regen.norecup[,2]))
fast.regen.sd = sd(na.omit(regen.norecup[,2]))
fast.test = mean(na.omit(test[,2]))
fast.test.sd = sd(na.omit(test[,2]))

cs1.mean = mean(na.omit(cs[,1])); cs1.sd = sd(na.omit(cs[,1]))
cs2.mean = mean(na.omit(cs[,2])); cs2.sd = sd(na.omit(cs[,2]))
cs3.mean = mean(na.omit(cs[,3])); cs3.sd = sd(na.omit(cs[,3]))

data.frame(bosl1s.regen, bosl1s.regen.sd, fast.regen, fast.regen.sd)
data.frame(bosl1s.test, bosl1s.test.sd, fast.test, fast.test.sd)
data.frame(cs1.mean, cs1.sd, cs2.mean, cs2.sd, cs3.mean, cs3.sd)
