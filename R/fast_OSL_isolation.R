library(Luminescence)
#library(xlsx)
rm(list = ls())
#graphics.off()

age = c(440,391,372,350,331,309,256,220)
subs = c(0.23,0.44,0.64,0.71,0.78,0.88,0.98,1.03)

setwd("C:/Users/iande/Documents/proyectos/Parnaiba/sec_bin/")
#cargar archivo .binx y leerlo
sample1 = "SensParnaiba_primeirolote_150209.binx"
sample2 = "SensParnaiba_segundolote_150210.binx"
sample3 = "SensParnaiba_terceirolote_150211.binx"
sample4 = "SensParnaiba_quartolote_150212.binx"
sample5 = "SensParnaiba_quintolote_150213.binx"
sample6 = "SensParnaiba_sextolote_150219.binx"
sample7 = "SensParnaiba_setimolote_150220.binx"
runmed.prop = 5
CW.1 = read_BIN2R(paste(sample1, sep = ""))
CW.1 = subset(CW.1, CW.1@METADATA$RUN == runmed.prop)
CW.1 = data.frame(CW.1@DATA)
CW.2 = read_BIN2R(paste(sample2, sep = ""))
CW.2 = subset(CW.2, CW.2@METADATA$RUN == runmed.prop)
CW.2 = data.frame(CW.2@DATA)
CW.3 = read_BIN2R(paste(sample3, sep = ""))
CW.3 = subset(CW.3, CW.3@METADATA$RUN == runmed.prop)
CW.3 = data.frame(CW.3@DATA)
CW.4 = read_BIN2R(paste(sample4, sep = ""))
CW.4 = subset(CW.4, CW.4@METADATA$RUN == runmed.prop)
CW.4 = data.frame(CW.4@DATA)
CW.5 = read_BIN2R(paste(sample5, sep = ""))
CW.5 = subset(CW.5, CW.5@METADATA$RUN == runmed.prop)
CW.5 = data.frame(CW.5@DATA)
CW.6 = read_BIN2R(paste(sample6, sep = ""))
CW.6 = subset(CW.6, CW.6@METADATA$RUN == runmed.prop)
CW.6 = data.frame(CW.6@DATA)
CW.7 = read_BIN2R(paste(sample7, sep = ""))
CW.7 = subset(CW.7, CW.7@METADATA$RUN == runmed.prop)
CW.7 = data.frame(CW.7@DATA)
#tiempo, canales, background
t = seq(0.25,100,0.25)
ch = 4; bg1 = 301; bg2 = 400

#calcular fast ratio###########################################################
fast.ratio.1 = 1:48; fast.ratio.2 = 1:48; fast.ratio.3 = 1:48; fast.ratio.4 = 1:48; fast.ratio.5 = 1:48; fast.ratio.6 = 1:48; fast.ratio.7 = 1:48
fast.ratio.se.1 = 1:48; fast.ratio.se.2 = 1:48; fast.ratio.se.3 = 1:48; fast.ratio.se.4 = 1:48; fast.ratio.se.5 = 1:48; fast.ratio.se.6 = 1:48; fast.ratio.se.7 = 1:48
for (i in 1:48) {
    #fr = as.data.frame(
    #cbind(
      fr.1 = calc_FastRatio(data.frame(t,CW.1[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                       stimulation.power = 40, wavelength = 470,
                       sigmaF = 2.32e-17,
                       sigmaM = 5.59e-18, plot = F)#))
      fast.ratio.1[i] = data.frame(fr.1$summary$fast.ratio)
      fast.ratio.se.1[i] = data.frame(fr.1$summary$fast.ratio.se)
      
      fr.2 = calc_FastRatio(data.frame(t,CW.2[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                            stimulation.power = 40, wavelength = 470,
                            sigmaF = 2.32e-17,
                            sigmaM = 5.59e-18, plot = F)#))
      fast.ratio.2[i] = data.frame(fr.2$summary$fast.ratio)
      fast.ratio.se.2[i] = data.frame(fr.2$summary$fast.ratio.se)
      
      fr.3 = calc_FastRatio(data.frame(t,CW.3[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                            stimulation.power = 40, wavelength = 470,
                            sigmaF = 2.32e-17,
                            sigmaM = 5.59e-18, plot = F)#))
      fast.ratio.3[i] = data.frame(fr.3$summary$fast.ratio)
      fast.ratio.se.3[i] = data.frame(fr.3$summary$fast.ratio.se)
      
      fr.4 = calc_FastRatio(data.frame(t,CW.4[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                            stimulation.power = 40, wavelength = 470,
                            sigmaF = 2.32e-17,
                            sigmaM = 5.59e-18, plot = F)#))
      fast.ratio.4[i] = data.frame(fr.4$summary$fast.ratio)
      fast.ratio.se.4[i] = data.frame(fr.4$summary$fast.ratio.se)
      
      fr.5 = calc_FastRatio(data.frame(t,CW.5[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                            stimulation.power = 40, wavelength = 470,
                            sigmaF = 2.32e-17,
                            sigmaM = 5.59e-18, plot = F)#))
      fast.ratio.5[i] = data.frame(fr.5$summary$fast.ratio)
      fast.ratio.se.5[i] = data.frame(fr.5$summary$fast.ratio.se)
      
      fr.6 = calc_FastRatio(data.frame(t,CW.6[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                            stimulation.power = 40, wavelength = 470,
                            sigmaF = 2.32e-17,
                            sigmaM = 5.59e-18, plot = F)#))
      fast.ratio.6[i] = data.frame(fr.6$summary$fast.ratio)
      fast.ratio.se.6[i] = data.frame(fr.6$summary$fast.ratio.se)
      
      fr.7 = calc_FastRatio(data.frame(t,CW.7[,i]), Ch_L1 = 1, Ch_L2 = ch, Ch_L3 = c(bg1,bg2), 
                            stimulation.power = 40, wavelength = 470,
                            sigmaF = 2.32e-17,
                            sigmaM = 5.59e-18, plot = F)#))
      fast.ratio.7[i] = data.frame(fr.7$summary$fast.ratio)
      fast.ratio.se.7[i] = data.frame(fr.7$summary$fast.ratio.se)
}

#calcular contribución###########################################
components = 3
#par(new = T, mfrow = c(2,2))
fit.data.1 = 1:48; fast.prop.1 = 1:48; fast.1 = 1:48; med.1 = 1:48; med.prop.1 = 1:48; slow.1 = 1:48; slow.prop.1 = 1:48; total.1 = 1:48
fit.data.2 = 1:48; fast.prop.2 = 1:48; fast.2 = 1:48; med.2 = 1:48; med.prop.2 = 1:48; slow.2 = 1:48; slow.prop.2 = 1:48; total.2 = 1:48
fit.data.3 = 1:48; fast.prop.3 = 1:48; fast.3 = 1:48; med.3 = 1:48; med.prop.3 = 1:48; slow.3 = 1:48; slow.prop.3 = 1:48; total.3 = 1:48
fit.data.4 = 1:48; fast.prop.4 = 1:48; fast.4 = 1:48; med.4 = 1:48; med.prop.4 = 1:48; slow.4 = 1:48; slow.prop.4 = 1:48; total.4 = 1:48
fit.data.5 = 1:48; fast.prop.5 = 1:48; fast.5 = 1:48; med.5 = 1:48; med.prop.5 = 1:48; slow.5 = 1:48; slow.prop.5 = 1:48; total.5 = 1:48
fit.data.6 = 1:48; fast.prop.6 = 1:48; fast.6 = 1:48; med.6 = 1:48; med.prop.6 = 1:48; slow.6 = 1:48; slow.prop.6 = 1:48; total.6 = 1:48
fit.data.7 = 1:48; fast.prop.7 = 1:48; fast.7 = 1:48; med.7 = 1:48; med.prop.7 = 1:48; slow.7 = 1:48; slow.prop.7 = 1:48; total.7 = 1:48
for (i in 1:48) {
  fit.1 = fit_CWCurve(data.frame(t,CW.1[,i]), n.components.max = components, fit.method = "LM",
            #log = "x",
            plot = T)
  fast.prop.1[i] = (sum(fit.1$component.contribution.matrix[[1]][,9][1:ch])/
                    (sum(fit.1$component.contribution.matrix[[1]][,9][1:ch])+
                    sum(fit.1$component.contribution.matrix[[1]][,10][1:ch])+
                    sum(fit.1$component.contribution.matrix[[1]][,11][1:ch])))*100
  med.prop.1[i] = (sum(fit.1$component.contribution.matrix[[1]][,10][1:ch])/
                   (sum(fit.1$component.contribution.matrix[[1]][,9][1:ch])+
                   sum(fit.1$component.contribution.matrix[[1]][,10][1:ch])+
                   sum(fit.1$component.contribution.matrix[[1]][,11][1:ch])))*100
  slow.prop.1[i] = (sum(fit.1$component.contribution.matrix[[1]][,11][1:ch])/
                    (sum(fit.1$component.contribution.matrix[[1]][,9][1:ch])+
                    sum(fit.1$component.contribution.matrix[[1]][,10][1:ch])+
                    sum(fit.1$component.contribution.matrix[[1]][,11][1:ch])))*100
  fast.1[i] = data.frame(fit.1$component.contribution.matrix[[1]][,9])
  med.1[i] = data.frame(fit.1$component.contribution.matrix[[1]][,10])
  slow.1[i] = data.frame(fit.1$component.contribution.matrix[[1]][,11])
}
for (i in 1:48) {  
  fit.2 = fit_CWCurve(data.frame(t,CW.2[,i]), n.components.max = components, fit.method = "LM",
                    #log = "x",
                    plot = T)
  fast.prop.2[i] = (sum(fit.2$component.contribution.matrix[[1]][,9][1:ch])/
                      (sum(fit.2$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.2$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.2$component.contribution.matrix[[1]][,11][1:ch])))*100
  med.prop.2[i] = (sum(fit.2$component.contribution.matrix[[1]][,10][1:ch])/
                     (sum(fit.2$component.contribution.matrix[[1]][,9][1:ch])+
                        sum(fit.2$component.contribution.matrix[[1]][,10][1:ch])+
                        sum(fit.2$component.contribution.matrix[[1]][,11][1:ch])))*100
  slow.prop.2[i] = (sum(fit.2$component.contribution.matrix[[1]][,11][1:ch])/
                      (sum(fit.2$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.2$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.2$component.contribution.matrix[[1]][,11][1:ch])))*100
  fast.2[i] = data.frame(fit.2$component.contribution.matrix[[1]][,9])
  med.2[i] = data.frame(fit.2$component.contribution.matrix[[1]][,10])
  slow.2[i] = data.frame(fit.2$component.contribution.matrix[[1]][,11])
}
for (i in c(1:33,35:48)) {
  fit.3 = fit_CWCurve(data.frame(t,CW.3[,i]), n.components.max = components, fit.method = "LM",
                    #log = "x",
                    plot = T)
  fast.prop.3[i] = (sum(fit.3$component.contribution.matrix[[1]][,9][1:ch])/
                      (sum(fit.3$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.3$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.3$component.contribution.matrix[[1]][,11][1:ch])))*100
  med.prop.3[i] = (sum(fit.3$component.contribution.matrix[[1]][,10][1:ch])/
                     (sum(fit.3$component.contribution.matrix[[1]][,9][1:ch])+
                        sum(fit.3$component.contribution.matrix[[1]][,10][1:ch])+
                        sum(fit.3$component.contribution.matrix[[1]][,11][1:ch])))*100
  slow.prop.3[i] = (sum(fit.3$component.contribution.matrix[[1]][,11][1:ch])/
                      (sum(fit.3$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.3$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.3$component.contribution.matrix[[1]][,11][1:ch])))*100
  fast.3[i] = data.frame(fit.3$component.contribution.matrix[[1]][,9])
  med.3[i] = data.frame(fit.3$component.contribution.matrix[[1]][,10])
  slow.3[i] = data.frame(fit.3$component.contribution.matrix[[1]][,11])
}
for (i in c(1:9,11:48)) {  
  fit.4 = fit_CWCurve(data.frame(t,CW.4[,i]), n.components.max = components, fit.method = "LM",
                    #log = "x",
                    plot = T)
  fast.prop.4[i] = (sum(fit.4$component.contribution.matrix[[1]][,9][1:ch])/
                      (sum(fit.4$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.4$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.4$component.contribution.matrix[[1]][,11][1:ch])))*100
  med.prop.4[i] = (sum(fit.4$component.contribution.matrix[[1]][,10][1:ch])/
                     (sum(fit.4$component.contribution.matrix[[1]][,9][1:ch])+
                        sum(fit.4$component.contribution.matrix[[1]][,10][1:ch])+
                        sum(fit.4$component.contribution.matrix[[1]][,11][1:ch])))*100
  slow.prop.4[i] = (sum(fit.4$component.contribution.matrix[[1]][,11][1:ch])/
                      (sum(fit.4$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.4$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.4$component.contribution.matrix[[1]][,11][1:ch])))*100
  fast.4[i] = data.frame(fit.4$component.contribution.matrix[[1]][,9])
  med.4[i] = data.frame(fit.4$component.contribution.matrix[[1]][,10])
  slow.4[i] = data.frame(fit.4$component.contribution.matrix[[1]][,11])
}
for (i in 1:48) {  
  fit.5 = fit_CWCurve(data.frame(t,CW.5[,i]), n.components.max = components, fit.method = "LM",
                    #log = "x",
                    plot = T)
  fast.prop.5[i] = (sum(fit.5$component.contribution.matrix[[1]][,9][1:ch])/
                      (sum(fit.5$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.5$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.5$component.contribution.matrix[[1]][,11][1:ch])))*100
  med.prop.5[i] = (sum(fit.5$component.contribution.matrix[[1]][,10][1:ch])/
                     (sum(fit.5$component.contribution.matrix[[1]][,9][1:ch])+
                        sum(fit.5$component.contribution.matrix[[1]][,10][1:ch])+
                        sum(fit.5$component.contribution.matrix[[1]][,11][1:ch])))*100
  slow.prop.5[i] = (sum(fit.5$component.contribution.matrix[[1]][,11][1:ch])/
                      (sum(fit.5$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.5$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.5$component.contribution.matrix[[1]][,11][1:ch])))*100
  fast.5[i] = data.frame(fit.5$component.contribution.matrix[[1]][,9])
  med.5[i] = data.frame(fit.5$component.contribution.matrix[[1]][,10])
  slow.5[i] = data.frame(fit.5$component.contribution.matrix[[1]][,11])
}
for (i in 1:48) {  
  fit.6 = fit_CWCurve(data.frame(t,CW.6[,i]), n.components.max = components, fit.method = "LM",
                    #log = "x",
                    plot = T)
  fast.prop.6[i] = (sum(fit.6$component.contribution.matrix[[1]][,9][1:ch])/
                      (sum(fit.6$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.6$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.6$component.contribution.matrix[[1]][,11][1:ch])))*100
  med.prop.6[i] = (sum(fit.6$component.contribution.matrix[[1]][,10][1:ch])/
                     (sum(fit.6$component.contribution.matrix[[1]][,9][1:ch])+
                        sum(fit.6$component.contribution.matrix[[1]][,10][1:ch])+
                        sum(fit.6$component.contribution.matrix[[1]][,11][1:ch])))*100
  slow.prop.6[i] = (sum(fit.6$component.contribution.matrix[[1]][,11][1:ch])/
                      (sum(fit.6$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.6$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.6$component.contribution.matrix[[1]][,11][1:ch])))*100
  fast.6[i] = data.frame(fit.6$component.contribution.matrix[[1]][,9])
  med.6[i] = data.frame(fit.6$component.contribution.matrix[[1]][,10])
  slow.6[i] = data.frame(fit.6$component.contribution.matrix[[1]][,11])
}
for (i in 1:48) {  
  fit.7 = fit_CWCurve(data.frame(t,CW.7[,i]), n.components.max = components, fit.method = "LM",
                    #log = "x",
                    plot = T)
  fast.prop.7[i] = (sum(fit.7$component.contribution.matrix[[1]][,9][1:ch])/
                      (sum(fit.7$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.7$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.7$component.contribution.matrix[[1]][,11][1:ch])))*100
  med.prop.7[i] = (sum(fit.7$component.contribution.matrix[[1]][,10][1:ch])/
                     (sum(fit.7$component.contribution.matrix[[1]][,9][1:ch])+
                        sum(fit.7$component.contribution.matrix[[1]][,10][1:ch])+
                        sum(fit.7$component.contribution.matrix[[1]][,11][1:ch])))*100
  slow.prop.7[i] = (sum(fit.7$component.contribution.matrix[[1]][,11][1:ch])/
                      (sum(fit.7$component.contribution.matrix[[1]][,9][1:ch])+
                         sum(fit.7$component.contribution.matrix[[1]][,10][1:ch])+
                         sum(fit.7$component.contribution.matrix[[1]][,11][1:ch])))*100
  fast.7[i] = data.frame(fit.7$component.contribution.matrix[[1]][,9])
  med.7[i] = data.frame(fit.7$component.contribution.matrix[[1]][,10])
  slow.7[i] = data.frame(fit.7$component.contribution.matrix[[1]][,11])
    #cont.matrix = data.frame(fast.prop, fast, med.prop, med, slow.prop, slow, total)
#dev.off()
}

#fast.prop#################################################################
#serra 14 muestras
fast.prop.bp75a.mean = mean(fast.prop.1[1:4])
fast.prop.bp86.mean = mean(fast.prop.1[5:8])
fast.prop.bp89.mean = mean(fast.prop.1[9:12])
fast.prop.bp92.mean = mean(fast.prop.1[13:16])
fast.prop.bp100.mean = mean(fast.prop.1[17:20])
fast.prop.bp106.mean = mean(fast.prop.1[21:24])
fast.prop.bp204.mean = mean(fast.prop.1[25:28])
fast.prop.bp207.mean = mean(fast.prop.1[29:32])
fast.prop.bp211.mean = mean(fast.prop.1[33:36])
fast.prop.bp214.mean = mean(fast.prop.1[37:40])
fast.prop.bp244.mean = mean(fast.prop.1[41:44])
fast.prop.bp87.mean = mean(fast.prop.7[37:40])
fast.prop.bp102.mean = mean(fast.prop.7[41:44])
fast.prop.bp104a.mean = mean(fast.prop.7[45:48])
fast.prop.bp75a.sd = sd(fast.prop.1[1:4])
fast.prop.bp86.sd = sd(fast.prop.1[5:8])
fast.prop.bp89.sd = sd(fast.prop.1[9:12])
fast.prop.bp92.sd = sd(fast.prop.1[13:16])
fast.prop.bp100.sd = sd(fast.prop.1[17:20])
fast.prop.bp106.sd = sd(fast.prop.1[21:24])
fast.prop.bp204.sd = sd(fast.prop.1[25:28])
fast.prop.bp207.sd = sd(fast.prop.1[29:32])
fast.prop.bp211.sd = sd(fast.prop.1[33:36])
fast.prop.bp214.sd = sd(fast.prop.1[37:40])
fast.prop.bp244.sd = sd(fast.prop.1[41:44])
fast.prop.bp87.sd = sd(fast.prop.7[37:40])
fast.prop.bp102.sd = sd(fast.prop.7[41:44])
fast.prop.bp104a.sd = sd(fast.prop.7[45:48])

#pimen 9 muestras
fast.prop.bp98.mean = mean(fast.prop.1[45:48]) 
fast.prop.bp165.mean = mean(fast.prop.2[1:4]) 
fast.prop.bp171.mean = mean(fast.prop.2[5:8])
fast.prop.bp181.mean = mean(fast.prop.2[9:12]) 
fast.prop.bp184.mean = mean(fast.prop.2[13:16])
fast.prop.bp203.mean = mean(fast.prop.2[17:20]) 
fast.prop.bp208.mean = mean(fast.prop.2[21:24])
fast.prop.bp210a.mean = mean(fast.prop.2[25:28])
fast.prop.bp219.mean = mean(fast.prop.2[29:32])
fast.prop.bp98.sd = sd(fast.prop.1[45:48]) 
fast.prop.bp165.sd = sd(fast.prop.2[1:4]) 
fast.prop.bp171.sd = sd(fast.prop.2[5:8])
fast.prop.bp181.sd = sd(fast.prop.2[9:12]) 
fast.prop.bp184.sd = sd(fast.prop.2[13:16])
fast.prop.bp203.sd = sd(fast.prop.2[17:20]) 
fast.prop.bp208.sd = sd(fast.prop.2[21:24])
fast.prop.bp210a.sd = sd(fast.prop.2[25:28])
fast.prop.bp219.sd = sd(fast.prop.2[29:32]) 

#cabe 15 muestras
fast.prop.bp72.mean = mean(fast.prop.2[33:36])
fast.prop.bp79.mean = mean(fast.prop.2[37:40])
fast.prop.bp94.mean = mean(fast.prop.2[41:44])
fast.prop.bp174.mean = mean(fast.prop.2[45:48])
fast.prop.bp177.mean = mean(fast.prop.3[1:4])
fast.prop.bp182.mean = mean(fast.prop.3[5:8])
fast.prop.bp189.mean = mean(fast.prop.3[9:12])
fast.prop.bp194.mean = mean(fast.prop.3[13:16])
fast.prop.bp202.mean = mean(fast.prop.3[17:20])
fast.prop.bp210b.mean = mean(fast.prop.3[21:24])
fast.prop.bp220.mean = mean(fast.prop.3[25:28])
fast.prop.bp78.mean = mean(fast.prop.7[21:24])
fast.prop.bp81a.mean = mean(fast.prop.7[25:28])
fast.prop.bp96.mean = mean(fast.prop.7[29:32])
fast.prop.bp175.mean = mean(fast.prop.7[33:36])
fast.prop.bp72.sd = sd(fast.prop.2[33:36])
fast.prop.bp79.sd = sd(fast.prop.2[37:40])
fast.prop.bp94.sd = sd(fast.prop.2[41:44])
fast.prop.bp174.sd = sd(fast.prop.2[45:48])
fast.prop.bp177.sd = sd(fast.prop.3[1:4])
fast.prop.bp182.sd = sd(fast.prop.3[5:8])
fast.prop.bp189.sd = sd(fast.prop.3[9:12])
fast.prop.bp194.sd = sd(fast.prop.3[13:16])
fast.prop.bp202.sd = sd(fast.prop.3[17:20])
fast.prop.bp210b.sd = sd(fast.prop.3[21:24])
fast.prop.bp220.sd = sd(fast.prop.3[25:28])
fast.prop.bp78.sd = sd(fast.prop.7[21:24])
fast.prop.bp81a.sd = sd(fast.prop.7[25:28])
fast.prop.bp96.sd = sd(fast.prop.7[29:32])
fast.prop.bp175.sd = sd(fast.prop.7[33:36])

#longa 9 muestras
fast.prop.bp83.mean = mean(fast.prop.3[29:32])
fast.prop.bp114.mean = mean(c(fast.prop.3[33],fast.prop.3[35:36]))
fast.prop.bp172.mean = mean(fast.prop.3[37:40])
fast.prop.bp173.mean = mean(fast.prop.3[41:44])
fast.prop.bp179.mean = mean(fast.prop.3[45:48])
fast.prop.bp180.mean = mean(fast.prop.4[1:4])
fast.prop.bp195.mean = mean(fast.prop.4[5:8])
fast.prop.bp198.mean = mean(c(fast.prop.4[9],fast.prop.4[11:12]))
fast.prop.bp223.mean = mean(fast.prop.4[13:16])
fast.prop.bp83.sd = sd(fast.prop.3[29:32])
fast.prop.bp114.sd = sd(c(fast.prop.3[33],fast.prop.3[35:36]))
fast.prop.bp172.sd = sd(fast.prop.3[37:40])
fast.prop.bp173.sd = sd(fast.prop.3[41:44])
fast.prop.bp179.sd = sd(fast.prop.3[45:48])
fast.prop.bp180.sd = sd(fast.prop.4[1:4])
fast.prop.bp195.sd = sd(fast.prop.4[5:8])
fast.prop.bp198.sd = sd(c(fast.prop.4[9],fast.prop.4[11:12]))
fast.prop.bp223.sd = sd(fast.prop.4[13:16])

#poti 14 muestras
fast.prop.bp82a.mean = mean(fast.prop.4[17:20])
fast.prop.bp82e.mean = mean(fast.prop.4[21:24])
fast.prop.bp82f.mean = mean(fast.prop.4[25:28])
fast.prop.bp82g.mean = mean(fast.prop.4[29:32])
fast.prop.bp82h.mean = mean(fast.prop.4[33:36])
fast.prop.bp82i.mean = mean(fast.prop.4[37:40])
fast.prop.bp131.mean = mean(fast.prop.4[41:44])
fast.prop.bp132.mean = mean(fast.prop.4[45:48])
fast.prop.bp136.mean = mean(fast.prop.5[1:4])
fast.prop.bp138.mean = mean(fast.prop.5[5:8])
fast.prop.bp140.mean = mean(fast.prop.5[9:12])
fast.prop.bp154.mean = mean(fast.prop.5[13:16])
fast.prop.bp226.mean = mean(fast.prop.5[17:20])
fast.prop.bp238.mean = mean(fast.prop.5[21:24])
fast.prop.bp82a.sd = sd(fast.prop.4[17:20])
fast.prop.bp82e.sd = sd(fast.prop.4[21:24])
fast.prop.bp82f.sd = sd(fast.prop.4[25:28])
fast.prop.bp82g.sd = sd(fast.prop.4[29:32])
fast.prop.bp82h.sd = sd(fast.prop.4[33:36])
fast.prop.bp82i.sd = sd(fast.prop.4[37:40])
fast.prop.bp131.sd = sd(fast.prop.4[41:44])
fast.prop.bp132.sd = sd(fast.prop.4[45:48])
fast.prop.bp136.sd = sd(fast.prop.5[1:4])
fast.prop.bp138.sd = sd(fast.prop.5[5:8])
fast.prop.bp140.sd = sd(fast.prop.5[9:12])
fast.prop.bp154.sd = sd(fast.prop.5[13:16])
fast.prop.bp226.sd = sd(fast.prop.5[17:20])
fast.prop.bp238.sd = sd(fast.prop.5[21:24])

#piaui 10 muestras
fast.prop.bp63.mean = mean(fast.prop.5[25:28])
fast.prop.bp68.mean = mean(fast.prop.5[29:32])
fast.prop.bp125.mean = mean(fast.prop.5[33:36])
fast.prop.bp139.mean = mean(fast.prop.5[37:40])
fast.prop.bp141.mean = mean(fast.prop.5[41:44])
fast.prop.bp150.mean = mean(fast.prop.5[45:48])
fast.prop.bp64.mean = mean(fast.prop.6[1:4])
fast.prop.bp67.mean = mean(fast.prop.6[5:8])
fast.prop.bp146b.mean = mean(fast.prop.6[9:12])
fast.prop.bp155.mean = mean(fast.prop.6[13:16])
fast.prop.bp63.sd = sd(fast.prop.5[25:28])
fast.prop.bp68.sd = sd(fast.prop.5[29:32])
fast.prop.bp125.sd = sd(fast.prop.5[33:36])
fast.prop.bp139.sd = sd(fast.prop.5[37:40])
fast.prop.bp141.sd = sd(fast.prop.5[41:44])
fast.prop.bp150.sd = sd(fast.prop.5[45:48])
fast.prop.bp64.sd = sd(fast.prop.6[1:4])
fast.prop.bp67.sd = sd(fast.prop.6[5:8])
fast.prop.bp146b.sd = sd(fast.prop.6[9:12])
fast.prop.bp155.sd = sd(fast.prop.6[13:16])

#motuca 5 muestras
fast.prop.bp133.mean = mean(fast.prop.6[17:20])
fast.prop.bp135.mean = mean(fast.prop.6[21:24])
fast.prop.bp143.mean = mean(fast.prop.6[25:28])
fast.prop.bp144.mean = mean(fast.prop.6[29:32])
fast.prop.185.mean = mean(fast.prop.6[33:36])
fast.prop.bp133.sd = sd(fast.prop.6[17:20])
fast.prop.bp135.sd = sd(fast.prop.6[21:24])
fast.prop.bp143.sd = sd(fast.prop.6[25:28])
fast.prop.bp144.sd = sd(fast.prop.6[29:32])
fast.prop.185.sd = sd(fast.prop.6[33:36])

#samba 8 muestras
fast.prop.bp120a.mean = mean(fast.prop.6[37:40])
fast.prop.bp121a.mean = mean(fast.prop.6[41:44])
fast.prop.bp121b.mean = mean(fast.prop.6[45:48])
fast.prop.bp121c.mean = mean(fast.prop.7[1:4])
fast.prop.bp121d.mean = mean(fast.prop.7[5:8])
fast.prop.bp121f.mean = mean(fast.prop.7[9:12])
fast.prop.bp121h.mean = mean(fast.prop.7[13:16])
fast.prop.bp123.mean = mean(fast.prop.7[17:20])
fast.prop.bp120a.sd = sd(fast.prop.6[37:40])
fast.prop.bp121a.sd = sd(fast.prop.6[41:44])
fast.prop.bp121b.sd = sd(fast.prop.6[45:48])
fast.prop.bp121c.sd = sd(fast.prop.7[1:4])
fast.prop.bp121d.sd = sd(fast.prop.7[5:8])
fast.prop.bp121f.sd = sd(fast.prop.7[9:12])
fast.prop.bp121h.sd = sd(fast.prop.7[13:16])
fast.prop.bp123.sd = sd(fast.prop.7[17:20])

#med.prop####################################################################
#serra 14 muestras
med.prop.bp75a.mean = mean(med.prop.1[1:4])
med.prop.bp86.mean = mean(med.prop.1[5:8])
med.prop.bp89.mean = mean(med.prop.1[9:12])
med.prop.bp92.mean = mean(med.prop.1[13:16])
med.prop.bp100.mean = mean(med.prop.1[17:20])
med.prop.bp106.mean = mean(med.prop.1[21:24])
med.prop.bp204.mean = mean(med.prop.1[25:28])
med.prop.bp207.mean = mean(med.prop.1[29:32])
med.prop.bp211.mean = mean(med.prop.1[33:36])
med.prop.bp214.mean = mean(med.prop.1[37:40])
med.prop.bp244.mean = mean(med.prop.1[41:44])
med.prop.bp87.mean = mean(med.prop.7[37:40])
med.prop.bp102.mean = mean(med.prop.7[41:44])
med.prop.bp104a.mean = mean(med.prop.7[45:48])
med.prop.bp75a.sd = sd(med.prop.1[1:4])
med.prop.bp86.sd = sd(med.prop.1[5:8])
med.prop.bp89.sd = sd(med.prop.1[9:12])
med.prop.bp92.sd = sd(med.prop.1[13:16])
med.prop.bp100.sd = sd(med.prop.1[17:20])
med.prop.bp106.sd = sd(med.prop.1[21:24])
med.prop.bp204.sd = sd(med.prop.1[25:28])
med.prop.bp207.sd = sd(med.prop.1[29:32])
med.prop.bp211.sd = sd(med.prop.1[33:36])
med.prop.bp214.sd = sd(med.prop.1[37:40])
med.prop.bp244.sd = sd(med.prop.1[41:44])
med.prop.bp87.sd = sd(med.prop.7[37:40])
med.prop.bp102.sd = sd(med.prop.7[41:44])
med.prop.bp104a.sd = sd(med.prop.7[45:48])

#pimen 9 muestras
med.prop.bp98.mean = mean(med.prop.1[45:48]) 
med.prop.bp165.mean = mean(med.prop.2[1:4]) 
med.prop.bp171.mean = mean(med.prop.2[5:8])
med.prop.bp181.mean = mean(med.prop.2[9:12]) 
med.prop.bp184.mean = mean(med.prop.2[13:16])
med.prop.bp203.mean = mean(med.prop.2[17:20]) 
med.prop.bp208.mean = mean(med.prop.2[21:24])
med.prop.bp210a.mean = mean(med.prop.2[25:28])
med.prop.bp219.mean = mean(med.prop.2[29:32])
med.prop.bp98.sd = sd(med.prop.1[45:48]) 
med.prop.bp165.sd = sd(med.prop.2[1:4]) 
med.prop.bp171.sd = sd(med.prop.2[5:8])
med.prop.bp181.sd = sd(med.prop.2[9:12]) 
med.prop.bp184.sd = sd(med.prop.2[13:16])
med.prop.bp203.sd = sd(med.prop.2[17:20]) 
med.prop.bp208.sd = sd(med.prop.2[21:24])
med.prop.bp210a.sd = sd(med.prop.2[25:28])
med.prop.bp219.sd = sd(med.prop.2[29:32]) 

#cabe 15 muestras
med.prop.bp72.mean = mean(med.prop.2[33:36])
med.prop.bp79.mean = mean(med.prop.2[37:40])
med.prop.bp94.mean = mean(med.prop.2[41:44])
med.prop.bp174.mean = mean(med.prop.2[45:48])
med.prop.bp177.mean = mean(med.prop.3[1:4])
med.prop.bp182.mean = mean(med.prop.3[5:8])
med.prop.bp189.mean = mean(med.prop.3[9:12])
med.prop.bp194.mean = mean(med.prop.3[13:16])
med.prop.bp202.mean = mean(med.prop.3[17:20])
med.prop.bp210b.mean = mean(med.prop.3[21:24])
med.prop.bp220.mean = mean(med.prop.3[25:28])
med.prop.bp78.mean = mean(med.prop.7[21:24])
med.prop.bp81a.mean = mean(med.prop.7[25:28])
med.prop.bp96.mean = mean(med.prop.7[29:32])
med.prop.bp175.mean = mean(med.prop.7[33:36])
med.prop.bp72.sd = sd(med.prop.2[33:36])
med.prop.bp79.sd = sd(med.prop.2[37:40])
med.prop.bp94.sd = sd(med.prop.2[41:44])
med.prop.bp174.sd = sd(med.prop.2[45:48])
med.prop.bp177.sd = sd(med.prop.3[1:4])
med.prop.bp182.sd = sd(med.prop.3[5:8])
med.prop.bp189.sd = sd(med.prop.3[9:12])
med.prop.bp194.sd = sd(med.prop.3[13:16])
med.prop.bp202.sd = sd(med.prop.3[17:20])
med.prop.bp210b.sd = sd(med.prop.3[21:24])
med.prop.bp220.sd = sd(med.prop.3[25:28])
med.prop.bp78.sd = sd(med.prop.7[21:24])
med.prop.bp81a.sd = sd(med.prop.7[25:28])
med.prop.bp96.sd = sd(med.prop.7[29:32])
med.prop.bp175.sd = sd(med.prop.7[33:36])

#longa 9 muestras
med.prop.bp83.mean = mean(med.prop.3[29:32])
med.prop.bp114.mean = mean(c(med.prop.3[33], fast.prop.3[35:36]))
med.prop.bp172.mean = mean(med.prop.3[37:40])
med.prop.bp173.mean = mean(med.prop.3[41:44])
med.prop.bp179.mean = mean(med.prop.3[45:48])
med.prop.bp180.mean = mean(med.prop.4[1:4])
med.prop.bp195.mean = mean(med.prop.4[5:8])
med.prop.bp198.mean = mean(c(med.prop.4[9], fast.prop.4[11:12]))
med.prop.bp223.mean = mean(med.prop.4[13:16])
med.prop.bp83.sd = sd(med.prop.3[29:32])
med.prop.bp114.sd = sd(c(med.prop.3[33], fast.prop.3[35:36]))
med.prop.bp172.sd = sd(med.prop.3[37:40])
med.prop.bp173.sd = sd(med.prop.3[41:44])
med.prop.bp179.sd = sd(med.prop.3[45:48])
med.prop.bp180.sd = sd(med.prop.4[1:4])
med.prop.bp195.sd = sd(med.prop.4[5:8])
med.prop.bp198.sd = sd(c(med.prop.4[9], fast.prop.4[11:12]))
med.prop.bp223.sd = sd(med.prop.4[13:16])

#poti 14 muestras
med.prop.bp82a.mean = mean(med.prop.4[17:20])
med.prop.bp82e.mean = mean(med.prop.4[21:24])
med.prop.bp82f.mean = mean(med.prop.4[25:28])
med.prop.bp82g.mean = mean(med.prop.4[29:32])
med.prop.bp82h.mean = mean(med.prop.4[33:36])
med.prop.bp82i.mean = mean(med.prop.4[37:40])
med.prop.bp131.mean = mean(med.prop.4[41:44])
med.prop.bp132.mean = mean(med.prop.4[45:48])
med.prop.bp136.mean = mean(med.prop.5[1:4])
med.prop.bp138.mean = mean(med.prop.5[5:8])
med.prop.bp140.mean = mean(med.prop.5[9:12])
med.prop.bp154.mean = mean(med.prop.5[13:16])
med.prop.bp226.mean = mean(med.prop.5[17:20])
med.prop.bp238.mean = mean(med.prop.5[21:24])
med.prop.bp82a.sd = sd(med.prop.4[17:20])
med.prop.bp82e.sd = sd(med.prop.4[21:24])
med.prop.bp82f.sd = sd(med.prop.4[25:28])
med.prop.bp82g.sd = sd(med.prop.4[29:32])
med.prop.bp82h.sd = sd(med.prop.4[33:36])
med.prop.bp82i.sd = sd(med.prop.4[37:40])
med.prop.bp131.sd = sd(med.prop.4[41:44])
med.prop.bp132.sd = sd(med.prop.4[45:48])
med.prop.bp136.sd = sd(med.prop.5[1:4])
med.prop.bp138.sd = sd(med.prop.5[5:8])
med.prop.bp140.sd = sd(med.prop.5[9:12])
med.prop.bp154.sd = sd(med.prop.5[13:16])
med.prop.bp226.sd = sd(med.prop.5[17:20])
med.prop.bp238.sd = sd(med.prop.5[21:24])

#piaui 10 muestras
med.prop.bp63.mean = mean(med.prop.5[25:28])
med.prop.bp68.mean = mean(med.prop.5[29:32])
med.prop.bp125.mean = mean(med.prop.5[33:36])
med.prop.bp139.mean = mean(med.prop.5[37:40])
med.prop.bp141.mean = mean(med.prop.5[41:44])
med.prop.bp150.mean = mean(med.prop.5[45:48])
med.prop.bp64.mean = mean(med.prop.6[1:4])
med.prop.bp67.mean = mean(med.prop.6[5:8])
med.prop.bp146b.mean = mean(med.prop.6[9:12])
med.prop.bp155.mean = mean(med.prop.6[13:16])
med.prop.bp63.sd = sd(med.prop.5[25:28])
med.prop.bp68.sd = sd(med.prop.5[29:32])
med.prop.bp125.sd = sd(med.prop.5[33:36])
med.prop.bp139.sd = sd(med.prop.5[37:40])
med.prop.bp141.sd = sd(med.prop.5[41:44])
med.prop.bp150.sd = sd(med.prop.5[45:48])
med.prop.bp64.sd = sd(med.prop.6[1:4])
med.prop.bp67.sd = sd(med.prop.6[5:8])
med.prop.bp146b.sd = sd(med.prop.6[9:12])
med.prop.bp155.sd = sd(med.prop.6[13:16])

#motuca 5 muestras
med.prop.bp133.mean = mean(med.prop.6[17:20])
med.prop.bp135.mean = mean(med.prop.6[21:24])
med.prop.bp143.mean = mean(med.prop.6[25:28])
med.prop.bp144.mean = mean(med.prop.6[29:32])
med.prop.185.mean = mean(med.prop.6[33:36])
med.prop.bp133.sd = sd(med.prop.6[17:20])
med.prop.bp135.sd = sd(med.prop.6[21:24])
med.prop.bp143.sd = sd(med.prop.6[25:28])
med.prop.bp144.sd = sd(med.prop.6[29:32])
med.prop.185.sd = sd(med.prop.6[33:36])

#samba 8 muestras
med.prop.bp120a.mean = mean(med.prop.6[37:40])
med.prop.bp121a.mean = mean(med.prop.6[41:44])
med.prop.bp121b.mean = mean(med.prop.6[45:48])
med.prop.bp121c.mean = mean(med.prop.7[1:4])
med.prop.bp121d.mean = mean(med.prop.7[5:8])
med.prop.bp121f.mean = mean(med.prop.7[9:12])
med.prop.bp121h.mean = mean(med.prop.7[13:16])
med.prop.bp123.mean = mean(med.prop.7[17:20])
med.prop.bp120a.sd = sd(med.prop.6[37:40])
med.prop.bp121a.sd = sd(med.prop.6[41:44])
med.prop.bp121b.sd = sd(med.prop.6[45:48])
med.prop.bp121c.sd = sd(med.prop.7[1:4])
med.prop.bp121d.sd = sd(med.prop.7[5:8])
med.prop.bp121f.sd = sd(med.prop.7[9:12])
med.prop.bp121h.sd = sd(med.prop.7[13:16])
med.prop.bp123.sd = sd(med.prop.7[17:20])
#slow.prop######################################################################
#serra 14 muestras
slow.prop.bp75a.mean = mean(slow.prop.1[1:4])
slow.prop.bp86.mean = mean(slow.prop.1[5:8])
slow.prop.bp89.mean = mean(slow.prop.1[9:12])
slow.prop.bp92.mean = mean(slow.prop.1[13:16])
slow.prop.bp100.mean = mean(slow.prop.1[17:20])
slow.prop.bp106.mean = mean(slow.prop.1[21:24])
slow.prop.bp204.mean = mean(slow.prop.1[25:28])
slow.prop.bp207.mean = mean(slow.prop.1[29:32])
slow.prop.bp211.mean = mean(slow.prop.1[33:36])
slow.prop.bp214.mean = mean(slow.prop.1[37:40])
slow.prop.bp244.mean = mean(slow.prop.1[41:44])
slow.prop.bp87.mean = mean(slow.prop.7[37:40])
slow.prop.bp102.mean = mean(slow.prop.7[41:44])
slow.prop.bp104a.mean = mean(slow.prop.7[45:48])
slow.prop.bp75a.sd = sd(slow.prop.1[1:4])
slow.prop.bp86.sd = sd(slow.prop.1[5:8])
slow.prop.bp89.sd = sd(slow.prop.1[9:12])
slow.prop.bp92.sd = sd(slow.prop.1[13:16])
slow.prop.bp100.sd = sd(slow.prop.1[17:20])
slow.prop.bp106.sd = sd(slow.prop.1[21:24])
slow.prop.bp204.sd = sd(slow.prop.1[25:28])
slow.prop.bp207.sd = sd(slow.prop.1[29:32])
slow.prop.bp211.sd = sd(slow.prop.1[33:36])
slow.prop.bp214.sd = sd(slow.prop.1[37:40])
slow.prop.bp244.sd = sd(slow.prop.1[41:44])
slow.prop.bp87.sd = sd(slow.prop.7[37:40])
slow.prop.bp102.sd = sd(slow.prop.7[41:44])
slow.prop.bp104a.sd = sd(slow.prop.7[45:48])

#pimen 9 muestras
slow.prop.bp98.mean = mean(slow.prop.1[45:48]) 
slow.prop.bp165.mean = mean(slow.prop.2[1:4]) 
slow.prop.bp171.mean = mean(slow.prop.2[5:8])
slow.prop.bp181.mean = mean(slow.prop.2[9:12]) 
slow.prop.bp184.mean = mean(slow.prop.2[13:16])
slow.prop.bp203.mean = mean(slow.prop.2[17:20]) 
slow.prop.bp208.mean = mean(slow.prop.2[21:24])
slow.prop.bp210a.mean = mean(slow.prop.2[25:28])
slow.prop.bp219.mean = mean(slow.prop.2[29:32])
slow.prop.bp98.sd = sd(slow.prop.1[45:48]) 
slow.prop.bp165.sd = sd(slow.prop.2[1:4]) 
slow.prop.bp171.sd = sd(slow.prop.2[5:8])
slow.prop.bp181.sd = sd(slow.prop.2[9:12]) 
slow.prop.bp184.sd = sd(slow.prop.2[13:16])
slow.prop.bp203.sd = sd(slow.prop.2[17:20]) 
slow.prop.bp208.sd = sd(slow.prop.2[21:24])
slow.prop.bp210a.sd = sd(slow.prop.2[25:28])
slow.prop.bp219.sd = sd(slow.prop.2[29:32]) 

#cabe 15 muestras
slow.prop.bp72.mean = mean(slow.prop.2[33:36])
slow.prop.bp79.mean = mean(slow.prop.2[37:40])
slow.prop.bp94.mean = mean(slow.prop.2[41:44])
slow.prop.bp174.mean = mean(slow.prop.2[45:48])
slow.prop.bp177.mean = mean(slow.prop.3[1:4])
slow.prop.bp182.mean = mean(slow.prop.3[5:8])
slow.prop.bp189.mean = mean(slow.prop.3[9:12])
slow.prop.bp194.mean = mean(slow.prop.3[13:16])
slow.prop.bp202.mean = mean(slow.prop.3[17:20])
slow.prop.bp210b.mean = mean(slow.prop.3[21:24])
slow.prop.bp220.mean = mean(slow.prop.3[25:28])
slow.prop.bp78.mean = mean(slow.prop.7[21:24])
slow.prop.bp81a.mean = mean(slow.prop.7[25:28])
slow.prop.bp96.mean = mean(slow.prop.7[29:32])
slow.prop.bp175.mean = mean(slow.prop.7[33:36])
slow.prop.bp72.sd = sd(slow.prop.2[33:36])
slow.prop.bp79.sd = sd(slow.prop.2[37:40])
slow.prop.bp94.sd = sd(slow.prop.2[41:44])
slow.prop.bp174.sd = sd(slow.prop.2[45:48])
slow.prop.bp177.sd = sd(slow.prop.3[1:4])
slow.prop.bp182.sd = sd(slow.prop.3[5:8])
slow.prop.bp189.sd = sd(slow.prop.3[9:12])
slow.prop.bp194.sd = sd(slow.prop.3[13:16])
slow.prop.bp202.sd = sd(slow.prop.3[17:20])
slow.prop.bp210b.sd = sd(slow.prop.3[21:24])
slow.prop.bp220.sd = sd(slow.prop.3[25:28])
slow.prop.bp78.sd = sd(slow.prop.7[21:24])
slow.prop.bp81a.sd = sd(slow.prop.7[25:28])
slow.prop.bp96.sd = sd(slow.prop.7[29:32])
slow.prop.bp175.sd = sd(slow.prop.7[33:36])

#longa 9 muestras
slow.prop.bp83.mean = mean(slow.prop.3[29:32])
slow.prop.bp114.mean = mean(c(med.prop.3[33], fast.prop.3[35:36]))
slow.prop.bp172.mean = mean(slow.prop.3[37:40])
slow.prop.bp173.mean = mean(slow.prop.3[41:44])
slow.prop.bp179.mean = mean(slow.prop.3[45:48])
slow.prop.bp180.mean = mean(slow.prop.4[1:4])
slow.prop.bp195.mean = mean(slow.prop.4[5:8])
slow.prop.bp198.mean = mean(c(med.prop.4[9], fast.prop.4[11:12]))
slow.prop.bp223.mean = mean(slow.prop.4[13:16])
slow.prop.bp83.sd = sd(slow.prop.3[29:32])
slow.prop.bp114.sd = sd(c(med.prop.3[33], fast.prop.3[35:36]))
slow.prop.bp172.sd = sd(slow.prop.3[37:40])
slow.prop.bp173.sd = sd(slow.prop.3[41:44])
slow.prop.bp179.sd = sd(slow.prop.3[45:48])
slow.prop.bp180.sd = sd(slow.prop.4[1:4])
slow.prop.bp195.sd = sd(slow.prop.4[5:8])
slow.prop.bp198.sd = sd(c(med.prop.4[9], fast.prop.4[11:12]))
slow.prop.bp223.sd = sd(slow.prop.4[13:16])

#poti 14 muestras
slow.prop.bp82a.mean = mean(slow.prop.4[17:20])
slow.prop.bp82e.mean = mean(slow.prop.4[21:24])
slow.prop.bp82f.mean = mean(slow.prop.4[25:28])
slow.prop.bp82g.mean = mean(slow.prop.4[29:32])
slow.prop.bp82h.mean = mean(slow.prop.4[33:36])
slow.prop.bp82i.mean = mean(slow.prop.4[37:40])
slow.prop.bp131.mean = mean(slow.prop.4[41:44])
slow.prop.bp132.mean = mean(slow.prop.4[45:48])
slow.prop.bp136.mean = mean(slow.prop.5[1:4])
slow.prop.bp138.mean = mean(slow.prop.5[5:8])
slow.prop.bp140.mean = mean(slow.prop.5[9:12])
slow.prop.bp154.mean = mean(slow.prop.5[13:16])
slow.prop.bp226.mean = mean(slow.prop.5[17:20])
slow.prop.bp238.mean = mean(slow.prop.5[21:24])
slow.prop.bp82a.sd = sd(slow.prop.4[17:20])
slow.prop.bp82e.sd = sd(slow.prop.4[21:24])
slow.prop.bp82f.sd = sd(slow.prop.4[25:28])
slow.prop.bp82g.sd = sd(slow.prop.4[29:32])
slow.prop.bp82h.sd = sd(slow.prop.4[33:36])
slow.prop.bp82i.sd = sd(slow.prop.4[37:40])
slow.prop.bp131.sd = sd(slow.prop.4[41:44])
slow.prop.bp132.sd = sd(slow.prop.4[45:48])
slow.prop.bp136.sd = sd(slow.prop.5[1:4])
slow.prop.bp138.sd = sd(slow.prop.5[5:8])
slow.prop.bp140.sd = sd(slow.prop.5[9:12])
slow.prop.bp154.sd = sd(slow.prop.5[13:16])
slow.prop.bp226.sd = sd(slow.prop.5[17:20])
slow.prop.bp238.sd = sd(slow.prop.5[21:24])

#piaui 10 muestras
slow.prop.bp63.mean = mean(slow.prop.5[25:28])
slow.prop.bp68.mean = mean(slow.prop.5[29:32])
slow.prop.bp125.mean = mean(slow.prop.5[33:36])
slow.prop.bp139.mean = mean(slow.prop.5[37:40])
slow.prop.bp141.mean = mean(slow.prop.5[41:44])
slow.prop.bp150.mean = mean(slow.prop.5[45:48])
slow.prop.bp64.mean = mean(slow.prop.6[1:4])
slow.prop.bp67.mean = mean(slow.prop.6[5:8])
slow.prop.bp146b.mean = mean(slow.prop.6[9:12])
slow.prop.bp155.mean = mean(slow.prop.6[13:16])
slow.prop.bp63.sd = sd(slow.prop.5[25:28])
slow.prop.bp68.sd = sd(slow.prop.5[29:32])
slow.prop.bp125.sd = sd(slow.prop.5[33:36])
slow.prop.bp139.sd = sd(slow.prop.5[37:40])
slow.prop.bp141.sd = sd(slow.prop.5[41:44])
slow.prop.bp150.sd = sd(slow.prop.5[45:48])
slow.prop.bp64.sd = sd(slow.prop.6[1:4])
slow.prop.bp67.sd = sd(slow.prop.6[5:8])
slow.prop.bp146b.sd = sd(slow.prop.6[9:12])
slow.prop.bp155.sd = sd(slow.prop.6[13:16])

#motuca 5 muestras
slow.prop.bp133.mean = mean(slow.prop.6[17:20])
slow.prop.bp135.mean = mean(slow.prop.6[21:24])
slow.prop.bp143.mean = mean(slow.prop.6[25:28])
slow.prop.bp144.mean = mean(slow.prop.6[29:32])
slow.prop.185.mean = mean(slow.prop.6[33:36])
slow.prop.bp133.sd = sd(slow.prop.6[17:20])
slow.prop.bp135.sd = sd(slow.prop.6[21:24])
slow.prop.bp143.sd = sd(slow.prop.6[25:28])
slow.prop.bp144.sd = sd(slow.prop.6[29:32])
slow.prop.185.sd = sd(slow.prop.6[33:36])

#samba 8 muestras
slow.prop.bp120a.mean = mean(slow.prop.6[37:40])
slow.prop.bp121a.mean = mean(slow.prop.6[41:44])
slow.prop.bp121b.mean = mean(slow.prop.6[45:48])
slow.prop.bp121c.mean = mean(slow.prop.7[1:4])
slow.prop.bp121d.mean = mean(slow.prop.7[5:8])
slow.prop.bp121f.mean = mean(slow.prop.7[9:12])
slow.prop.bp121h.mean = mean(slow.prop.7[13:16])
slow.prop.bp123.mean = mean(slow.prop.7[17:20])
slow.prop.bp120a.sd = sd(slow.prop.6[37:40])
slow.prop.bp121a.sd = sd(slow.prop.6[41:44])
slow.prop.bp121b.sd = sd(slow.prop.6[45:48])
slow.prop.bp121c.sd = sd(slow.prop.7[1:4])
slow.prop.bp121d.sd = sd(slow.prop.7[5:8])
slow.prop.bp121f.sd = sd(slow.prop.7[9:12])
slow.prop.bp121h.sd = sd(slow.prop.7[13:16])
slow.prop.bp123.sd = sd(slow.prop.7[17:20])
#hacer las tablas con los datos de intensidad fast.prop###################
fast.prop.serra = data.frame(c(fast.prop.bp75a.mean,fast.prop.bp86.mean,fast.prop.bp89.mean,fast.prop.bp92.mean,
                         fast.prop.bp100.mean,fast.prop.bp106.mean,fast.prop.bp204.mean,fast.prop.bp207.mean,
                         fast.prop.bp211.mean,fast.prop.bp214.mean,fast.prop.bp244.mean,fast.prop.bp87.mean,
                         fast.prop.bp102.mean,fast.prop.bp104a.mean))
fast.prop.pimen = data.frame(c(fast.prop.bp98.mean,
                         fast.prop.bp165.mean,fast.prop.bp171.mean,fast.prop.bp181.mean,fast.prop.bp184.mean,
                         fast.prop.bp203.mean,fast.prop.bp208.mean,fast.prop.bp210a.mean,
                         fast.prop.bp219.mean))
fast.prop.cabe = data.frame(c(fast.prop.bp72.mean,fast.prop.bp79.mean,
                        fast.prop.bp94.mean,
                        fast.prop.bp174.mean,fast.prop.bp177.mean,fast.prop.bp182.mean,
                        fast.prop.bp189.mean,fast.prop.bp194.mean,
                        fast.prop.bp202.mean,
                        fast.prop.bp210b.mean,fast.prop.bp220.mean,fast.prop.bp78.mean,
                        fast.prop.bp81a.mean,fast.prop.bp96.mean,fast.prop.bp175.mean))
fast.prop.longa = data.frame(c(fast.prop.bp83.mean,fast.prop.bp114.mean,
                         fast.prop.bp172.mean,fast.prop.bp173.mean,fast.prop.bp179.mean,fast.prop.bp180.mean,
                         fast.prop.bp195.mean,fast.prop.bp198.mean,
                         fast.prop.bp223.mean))
fast.prop.poti = data.frame(c(fast.prop.bp82a.mean,fast.prop.bp82e.mean,fast.prop.bp82f.mean,fast.prop.bp82g.mean,
                        fast.prop.bp82h.mean,fast.prop.bp82i.mean,
                        fast.prop.bp131.mean,fast.prop.bp132.mean,fast.prop.bp136.mean,fast.prop.bp138.mean,fast.prop.bp140.mean,fast.prop.bp154.mean,
                        fast.prop.bp226.mean,fast.prop.bp238.mean))
fast.prop.piaui = data.frame(c(fast.prop.bp63.mean,fast.prop.bp68.mean,
                         fast.prop.bp125.mean,fast.prop.bp139.mean,fast.prop.bp141.mean,fast.prop.bp150.mean,
                         fast.prop.bp64.mean,fast.prop.bp67.mean,
                         fast.prop.bp146b.mean,fast.prop.bp155.mean))
fast.prop.motuca = data.frame(c(fast.prop.bp133.mean,fast.prop.bp135.mean,fast.prop.bp143.mean,fast.prop.bp144.mean,
                          fast.prop.185.mean))
fast.prop.samba = data.frame(c(fast.prop.bp120a.mean,fast.prop.bp121a.mean,fast.prop.bp121b.mean,fast.prop.bp121c.mean,
                         fast.prop.bp121d.mean,fast.prop.bp121f.mean,fast.prop.bp121h.mean,fast.prop.bp123.mean))

#hacer las tablas con los datos de intensidad med.prop############
med.prop.serra = data.frame(c(med.prop.bp75a.mean,med.prop.bp86.mean,med.prop.bp89.mean,med.prop.bp92.mean,
                         med.prop.bp100.mean,med.prop.bp106.mean,med.prop.bp204.mean,med.prop.bp207.mean,
                         med.prop.bp211.mean,med.prop.bp214.mean,med.prop.bp244.mean,med.prop.bp87.mean,
                         med.prop.bp102.mean,med.prop.bp104a.mean))
med.prop.pimen = data.frame(c(med.prop.bp98.mean,
                         med.prop.bp165.mean,med.prop.bp171.mean,med.prop.bp181.mean,med.prop.bp184.mean,
                         med.prop.bp203.mean,med.prop.bp208.mean,med.prop.bp210a.mean,
                         med.prop.bp219.mean))
med.prop.cabe = data.frame(c(med.prop.bp72.mean,med.prop.bp79.mean,
                        med.prop.bp94.mean,
                        med.prop.bp174.mean,med.prop.bp177.mean,med.prop.bp182.mean,
                        med.prop.bp189.mean,med.prop.bp194.mean,
                        med.prop.bp202.mean,
                        med.prop.bp210b.mean,med.prop.bp220.mean,med.prop.bp78.mean,
                        med.prop.bp81a.mean,med.prop.bp96.mean,med.prop.bp175.mean))
med.prop.longa = data.frame(c(med.prop.bp83.mean,med.prop.bp114.mean,
                         med.prop.bp172.mean,med.prop.bp173.mean,med.prop.bp179.mean,med.prop.bp180.mean,
                         med.prop.bp195.mean,med.prop.bp198.mean,
                         med.prop.bp223.mean))
med.prop.poti = data.frame(c(med.prop.bp82a.mean,med.prop.bp82e.mean,med.prop.bp82f.mean,med.prop.bp82g.mean,
                        med.prop.bp82h.mean,med.prop.bp82i.mean,
                        med.prop.bp131.mean,med.prop.bp132.mean,med.prop.bp136.mean,med.prop.bp138.mean,med.prop.bp140.mean,med.prop.bp154.mean,
                        med.prop.bp226.mean,med.prop.bp238.mean))
med.prop.piaui = data.frame(c(med.prop.bp63.mean,med.prop.bp68.mean,
                         med.prop.bp125.mean,med.prop.bp139.mean,med.prop.bp141.mean,med.prop.bp150.mean,
                         med.prop.bp64.mean,med.prop.bp67.mean,
                         med.prop.bp146b.mean,med.prop.bp155.mean))
med.prop.motuca = data.frame(c(med.prop.bp133.mean,med.prop.bp135.mean,med.prop.bp143.mean,med.prop.bp144.mean,
                          med.prop.185.mean))
med.prop.samba = data.frame(c(med.prop.bp120a.mean,med.prop.bp121a.mean,med.prop.bp121b.mean,med.prop.bp121c.mean,
                         med.prop.bp121d.mean,med.prop.bp121f.mean,med.prop.bp121h.mean,med.prop.bp123.mean))

#hacer las tablas con los datos de intensidad slow.prop###############
slow.prop.serra = data.frame(c(slow.prop.bp75a.mean,slow.prop.bp86.mean,slow.prop.bp89.mean,slow.prop.bp92.mean,
                         slow.prop.bp100.mean,slow.prop.bp106.mean,slow.prop.bp204.mean,slow.prop.bp207.mean,
                         slow.prop.bp211.mean,slow.prop.bp214.mean,slow.prop.bp244.mean,slow.prop.bp87.mean,
                         slow.prop.bp102.mean,slow.prop.bp104a.mean))
slow.prop.pimen = data.frame(c(slow.prop.bp98.mean,
                         slow.prop.bp165.mean,slow.prop.bp171.mean,slow.prop.bp181.mean,slow.prop.bp184.mean,
                         slow.prop.bp203.mean,slow.prop.bp208.mean,slow.prop.bp210a.mean,
                         slow.prop.bp219.mean))
slow.prop.cabe = data.frame(c(slow.prop.bp72.mean,slow.prop.bp79.mean,
                        slow.prop.bp94.mean,
                        slow.prop.bp174.mean,slow.prop.bp177.mean,slow.prop.bp182.mean,
                        slow.prop.bp189.mean,slow.prop.bp194.mean,
                        slow.prop.bp202.mean,
                        slow.prop.bp210b.mean,slow.prop.bp220.mean,slow.prop.bp78.mean,
                        slow.prop.bp81a.mean,slow.prop.bp96.mean,slow.prop.bp175.mean))
slow.prop.longa = data.frame(c(slow.prop.bp83.mean,slow.prop.bp114.mean,
                         slow.prop.bp172.mean,slow.prop.bp173.mean,slow.prop.bp179.mean,slow.prop.bp180.mean,
                         slow.prop.bp195.mean,slow.prop.bp198.mean,
                         slow.prop.bp223.mean))
slow.prop.poti = data.frame(c(slow.prop.bp82a.mean,slow.prop.bp82e.mean,slow.prop.bp82f.mean,slow.prop.bp82g.mean,
                        slow.prop.bp82h.mean,slow.prop.bp82i.mean,
                        slow.prop.bp131.mean,slow.prop.bp132.mean,slow.prop.bp136.mean,slow.prop.bp138.mean,slow.prop.bp140.mean,slow.prop.bp154.mean,
                        slow.prop.bp226.mean,slow.prop.bp238.mean))
slow.prop.piaui = data.frame(c(slow.prop.bp63.mean,slow.prop.bp68.mean,
                         slow.prop.bp125.mean,slow.prop.bp139.mean,slow.prop.bp141.mean,slow.prop.bp150.mean,
                         slow.prop.bp64.mean,slow.prop.bp67.mean,
                         slow.prop.bp146b.mean,slow.prop.bp155.mean))
slow.prop.motuca = data.frame(c(slow.prop.bp133.mean,slow.prop.bp135.mean,slow.prop.bp143.mean,slow.prop.bp144.mean,
                          slow.prop.185.mean))
slow.prop.samba = data.frame(c(slow.prop.bp120a.mean,slow.prop.bp121a.mean,slow.prop.bp121b.mean,slow.prop.bp121c.mean,
                         slow.prop.bp121d.mean,slow.prop.bp121f.mean,slow.prop.bp121h.mean,slow.prop.bp123.mean))

serra = data.frame(x = 1:nrow(nat.serra), age = rep(age[1], length(nrow(nat.serra))), subs = rep(subs[1], length(nrow(nat.serra))),  nat.serra, itl.serra, irsl.serra, osl.serra, tl300.serra, tl110.serra, tl300pos.serra, tl110pos.serra)
colnames(serra) = c("x", "age", "subs", "nat", "itl", "irsl", "osl", "tl300", "tl110", "tl300pos", "tl110pos")
mserra = reshape2::melt(serra, id.var = "x")
#serra$age = as.numeric(levels(serra$age))[serra$age]
#serra$subs = as.numeric(levels(serra$subs))[serra$subs]
#graficos####################################################################
par(mar = c(4.5,4.5,2,1.5))

#contribution of each component by time
matplot(t, cbind(fast.3, med.3, slow.3), type = "l", log = "x", xlim = c(0.25,100), ylim = c(0,100),
        ylab = "Contribution %", xlab = "Time (s)", xaxs = "i", yaxs = "i")

#contribution of each component to the first second
matplot(cbind(fast.prop.3, med.prop.3, slow.prop.3), type = "p", pch = c(16:18), 
        col = c("#4467a4","#f49d36", "#52b79e"),
        xlim = c(0,50), ylim = c(0,100),
        ylab = "Contribution %", xlab = "Aliquot", xaxs = "i", yaxs = "i")
legend(x = 30, y = 60, col = c("#4467a4","#f49d36","#52b79e"), 
       c("Fast","Medium","Slow"), lty = 1, lwd = 3, cex = 1.5)

#cambia esto para que salga solo las cuatro curvas (fast, med, slow, total) de una alicuota
matplot(t, cbind(fast.prop.1, med.prop.1, slow.prop.1), type = "l", lwd = 3, lty = 1,
        cex = 1, cex.lab = 2, cex.axis = 2, xaxs = "i", yaxs = "i", log = "x",
        col = c("#4467a4","#f49d36", "#e94e1c"), xlab = "Time (s)", ylab = "Contribution of each comp (%)")
legend(x = min(t), y = max(slow.prop), col = c("#4467a4","#f49d36", "#e94e1c"), 
       c("Fast","Medium","Slow"), lty = 1, lwd = 3, cex = 1.5)
#}
#suma de comp rapida y de todas las componentes para los primeros 0.8s
#sum(fast[1:ch]); sum(fast[1:ch], med[1:ch], slow[1:ch])
#suma del total de toda la curva
#sum(fast[1:bg2], med[1:bg2], slow[1:bg2])
#sum(fast[1:ch])/sum(fast[1:bg2], med[1:bg2], slow[1:bg2])
#sum(fast[1:ch], med[1:ch], slow[1:ch])/sum(fast[1:bg2], med[1:bg2], slow[1:bg2])
#proporción de componente rápida para los primeros ch canales
#fast.cont = data.frame(100*sum(fast[,i][1:ch])/sum(fast[,i][1:ch], med[,i][1:ch], slow[,i][1:ch]))
#text(x = 40, y = 0.9*max(sum(fast[1],med[1],slow[1])), 
#     paste(round(100*sum(fast[1:ch])/sum(fast[1:ch], med[1:ch], slow[1:ch]), 1), "% of fast component"),
#     cex = 1.7)
#text(x = 40, y = 0.8*max(sum(fast[1],med[1],slow[1])),
#     paste("Fast ratio = ", round(fr,1)), cex = 1.7)
#data = data.frame(read.xlsx("fast_isolation.xlsx", sheetName = "Hoja1"))
#plot_GrowthCurve(data)

