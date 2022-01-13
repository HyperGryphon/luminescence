rm(list = ls())
graphics.off()

library(Luminescence); library(xlsx)
#chose directory
setwd("C:/Users/iande/Documentos/proyectos/thermochron/binfiles/")
#chose sample
sample = "Sens_L1444-81-82-83-84_R1_200331.binx"
aliquot = 1
run = 6

#change run number depending on the position of the OSL sens sequence
CW = read_BIN2R(paste(sample, sep = ""))
CW = subset(CW, CW@METADATA$RUN == run)
CW = data.frame(CW@DATA)

#select channels of integration and background
t = seq(0.1,40,0.1)
ch = 10; bg1 = 301; bg2 = 400

#how many components, max = 4
components = 3

########################################################################
################ just press ctrl+A and enter to run ####################
########################################################################

#sensitivity calculation 
osl = 1:length(CW); bg.osl = 1:length(CW)
sens.osl = 1:length(CW); bg.tot = 1:length(CW)
for (i in 1:length(CW)) {
  # OSL in cts/1s
  osl[i] = sum(CW[,i][1:ch])-mean(CW[,i][bg1:bg2])*ch
  # %BOSLF
  sens.osl[i] = osl[i]/(sum(CW[,i][1:bg2])-mean(CW[,i][bg1:bg2])*bg2)*100
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
if (components == 4) {
  fast.rec = (fast.prop/(fast.prop+med.prop+slow.prop))*100
  med.rec = (med.prop/(fast.prop+med.prop+slow.prop))*100
  slow.rec = (slow.prop/(fast.prop+med.prop+slow.prop))*100
  fast.prop = fast.rec; med.prop = med.rec; slow.prop = slow.rec
  fast.ratio = cbind(fast.ratio, fast.ratio.se)
  comp.prop = data.frame(sens.osl, fast.prop, med.prop, slow.prop)
  colnames(comp.prop) = c("%BOSLf","fast (% of BOSLf)", "medium (% of BOSLf)", "slow (% of BOSLf)")
  osl.comp = data.frame(osl, osl*fast.prop/100, osl*med.prop/100, osl*slow.prop/100)
  colnames(osl.comp) = c("OSL (cts/1s)","fast (cts/1s)", "med (cts/1s)", "slow (cts/1s)")
  comp = data.frame(mean(fast.prop), mean(na.omit(med.prop)), mean(na.omit(slow.prop)))
  comp.sd = data.frame(sd(fast.prop), sd(na.omit(med.prop)), sd(na.omit(slow.prop)))
  comp.se = data.frame(sd(fast.prop)/(sqrt(length(fast.prop))), 
                       sd(na.omit(med.prop))/(sqrt(length(med.prop))), 
                     sd(na.omit(slow.prop))/(sqrt(length(slow.prop))))
  table = cbind(comp.prop, osl.comp, fast.ratio)
}

#plot results
plot(1:length(CW),fast.ratio[,1])
par(mar = c(4,4,4,6), xpd = T)
matplot(cbind(fast.prop, med.prop, slow.prop), pch = 15:17, col = 2:4,
        xlab = "aliquot", ylab = "% proportion")
legend("topright", inset = c(-0.15, 0), pch = 15:17, col = 2:4,
       legend = c("Fast", "Medium", "Slow"))

#create excel file
write.xlsx(table, file = paste0(sample, ".xlsx"), sheetName = paste0("run ", run), append = T)

