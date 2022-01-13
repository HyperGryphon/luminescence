rm(list = ls())
#graphics.off()

library(Luminescence); library(tibble); library(tidyverse); library(xlsx)
#chose directory
#setwd("C:/Users/iande/Documentos/proyectos/fernanda/binfiles/")
#chose sample
sample <- file.choose()
#sample <- "Sens_Polimin_155854-49-23-97-68-58-80-63-86_R1_210705.binx"

#samples name and how many aliquots per sample
samples <- c(rep("155816", 5), rep("155812", 5), rep("155813", 5), rep("155814", 5),
            rep("155815", 5), rep("155831", 5), rep("155842", 5), rep("155851", 5),
            rep("155856", 5))

#units name and how many samples per unit
#units <- c(rep("Amazon", 8), rep("Xingu", 8))

#CoreAutazes sensitivity sequence
runirsl <- 4; runosl <- 5; runtl325 <- 6; runtl110 <- 8; runtlbg <- 9

#select channels of integration and background for deconvolution
t <- seq(0.25,100,0.25)
#integration intervals, check channels/s for sensitivity calculation
irslint <- 1:3; irslbg <- 721:750; irsltot <- 1:750
oslint <- 1:2; oslbg <- 361:400; osltot <- 1:400
#osl early bg
oslebg <- 3:4

#how many components, max <- 4
components <- 3
#Do you want to plot the deconvolution graphs?
plot.deconv <- F

#How many degrees per channel for TL signals
degree <- 2
#Select TL points up to 400 ºC
temps <- tl325tot <- tl110tot <- 1:(500/degree)
#integration time intervals for TL sensitivity
tl325int <- tl325bg <- (200/degree):(380/degree)
tl110int <- tl110bg <- (80/degree):(115/degree)
#temperature interval for TL curves
tint <- 10 #temp in degrees * cts/degree


########################################################################
################ just press ctrl+A and enter to run ####################
########################################################################

data <- read_BIN2R(paste(sample, sep = ""))
n.aliquots <- length(unique(data@METADATA$POSITION))
n.samples <- 9

#Select signals
irsl <- subset(data, data@METADATA$RUN == runirsl)
osl <- subset(data, data@METADATA$RUN == runosl)
tl325 <- subset(data, data@METADATA$RUN == runtl325)
tl325pos <- subset(data, data@METADATA$RUN == runtl325)
tl110 <- subset(data, data@METADATA$RUN == runtl110)
tl110pos <- subset(data, data@METADATA$RUN == runtl110)# & data@METADATA$AN_TEMP==220 & data@METADATA$RUN == 6)
tlbg <- subset(data, data@METADATA$RUN == runtlbg)

#Estimate doses
dose.rate <- data.frame(data@METADATA$IRR_DOSERATE); dose.time <- data.frame(data@METADATA$IRR_TIME)
doses <- data.frame(dose.rate*dose.time)
colnames(doses) <- c("Dose (Gy)")

irsl <- data.frame(irsl@DATA)
osl <- data.frame(osl@DATA)
tl325 <- data.frame(tl325@DATA)
tl110 <- data.frame(tl110@DATA)
tlbg <- data.frame(tlbg@DATA)

osl.s <- ebg.osl <- bg.osl <- 1:n.aliquots
osl.total <- bg.total <- 1:n.aliquots
sens.osl <- osl.cts <- 1:n.aliquots
sens <- sd.bg.osl <- sd.ebg.osl <- 1:n.aliquots
for (i in 1:n.aliquots) {
  
  osl.s[i] <- sum(osl[oslint,i])
  osl.total[i] <- sum(osl[osltot,i])
  bg.osl[i] <- mean(osl[oslbg,i])*length(osl[oslint,i])
  ebg.osl[i] <- mean(osl[oslebg,i])*length(osl[oslint,i])
  sd.bg.osl[i] <- sd(osl[oslbg,i])
  sd.ebg.osl[i] <- sd(osl[oslebg,i])
  bg.total[i] <- mean(osl[oslbg,i])*length(osl[osltot,i])
  # OSL in cts/1s
  osl.cts[i] <- (osl.s[i]-ebg.osl[i]) #/(doses[i]*8.9) #cts/Gy·mg
  # BOSLF
  sens.osl[i] <- (osl.s[i]-ebg.osl[i])/(osl.total[i]-bg.total[i])*100
  
  if (sens.osl[i] < 0) {
    sens[i] <- 0
  } 
  else if (osl.cts[i] < (ebg.osl[i]+3*sd.ebg.osl[i])) {
    sens[i] <- 0
  } 
  #else  if (osl[i] > (bg.osl[i]+3*sd.bg.osl[i]) & osl[i] < (2*bg.osl[i]+3*sd.bg.osl[i])) {
  #sens[i] <- print("low")
  #}
  else {
    sens[i] <- sens.osl[i]
  }
}

sens.irsl <- 1:n.aliquots; irsl.cts <- 1:n.aliquots
for (i in 1:n.aliquots) {
  irsl.cts[i] <- sum(irsl[irslint,i])-mean(irsl[irslbg,i]*max(irslint))
  sens.irsl[i] <- (sum(irsl[irslint,i])-mean(irsl[irslbg,i]*max(irslint)))/(sum(osl[oslint,i])-mean(osl[oslbg,i]*max(oslint)))*100
}

fast.prop <- 1:n.aliquots; med.prop <- 1:n.aliquots; slow.prop <- 1:n.aliquots; slow.2.prop <- 1:n.aliquots
fast <- 1:n.aliquots; med <- 1:n.aliquots; slow <- 1:n.aliquots; slow.2 <- 1:n.aliquots
for (i in 1:n.aliquots) {
  fit <- fit_CWCurve(data.frame(t,osl[,i]), n.components.max = components, fit.method = "LM",
                    fit.trace = F, fit.failure_threshold = T, 
                    LED.power = 40, LED.wavelength = 470,
                    #log = "x",
                    plot = plot.deconv)
  if (ncol(fit$component.contribution.matrix[[1]]) == 15) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])/
                     (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                        sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                        sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                        sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    slow.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    slow.2.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])/
                        (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                           sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                           sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])+
                           sum(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])))*100
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])
    slow[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])
    slow.2[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c4"])
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 12) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])/
                     (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                        sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                        sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])))*100
    slow.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])))*100
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])
    slow[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c3"])
    slow.2[i] <- data.frame(print(NA))
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 9) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                         sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])/
                     (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])+
                        sum(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])))*100
    slow.prop[i] <- print(NA)
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c2"])
    slow[i] <- data.frame(print(NA))
    slow.2[i] <- data.frame(print(NA))
  }
  if(ncol(fit$component.contribution.matrix[[1]]) == 6) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])/
                      (sum(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])))*100
    med.prop[i] <- print(NA)
    slow.prop[i] <- print(NA)
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][oslint,"cont.c1"])
    med[i] <- data.frame(print(NA))
    slow[i] <- data.frame(print(NA))
    slow.2[i] <- data.frame(print(NA))
  }
}

sens.tl325 <- tl325.cts <- tl325pos <- 1:n.aliquots
tl325pos.ini <- tl325pos.fin <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl325pos[i] <- which.max(abs(tl325[temps,i]))
  tl325pos.ini[i] <- which(tl325[i] == tl325[(tl325pos[i]-tint*degree),i])
  tl325pos.fin[i] <- which(tl325[i] == tl325[(tl325pos[i]+tint*degree),i])
  tl325.cts[i] <- sum(tl325[temps,i][tl325pos.ini[i]:tl325pos.fin[i]])-sum(tlbg[temps,i][tl325pos.ini[i]:tl325pos.fin[i]])
  sens.tl325[i] <- ((sum(tl325[tl325pos.ini[i]:tl325pos.fin[i],i])-sum(tlbg[tl325pos.ini[i]:tl325pos.fin[i],i]))/(sum(tl325[temps,i])-sum(tlbg[temps,i])))*100 
}
tl325pos <- tl325pos*degree-degree

tl110pos <- sens.tl110 <- tl110.cts <- 1:n.aliquots
tl110pos.ini <- tl110pos.fin <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl110pos[i] <- which.max(abs(tl110[temps,i]))
  tl110pos.ini[i] <- which(tl110[i] == tl110[(tl110pos[i]-tint*degree),i])
  tl110pos.fin[i] <- which(tl110[i] == tl110[(tl110pos[i]+tint*degree),i])
  tl110.cts[i] <- sum(tl110[temps,i][tl110pos.ini[i]:tl110pos.fin[i]])-sum(tlbg[temps,i][tl110pos.ini[i]:tl110pos.fin[i]])
  sens.tl110[i] <- ((sum(tl110[tl110pos.ini[i]:tl110pos.fin[i],i])-sum(tlbg[tl110pos.ini[i]:tl110pos.fin[i],i]))/(sum(tl110[temps,i])-sum(tlbg[temps,i])))*100 
}
tl110pos <- tl110pos*degree-degree

#condense data
data <- tibble(samples, sens.irsl, irsl.cts, sens.osl, fast.prop, med.prop, slow.prop, osl.cts, sens.tl325, tl325.cts, tl325pos, sens.tl110, tl110.cts, tl110pos)
table.samples <- aggregate(data[2:length(data)], list(data$samples), mean, na.rm = T)
table.samples <- tibble(table.samples)
table.samples.sd <- aggregate(data[2:length(data)], list(data$samples), sd, na.rm = T)
table.samples.sd <- tibble(table.samples.sd)
#table.units <- aggregate(data[3:length(data)], list(data$units), mean, na.rm = T)
#table.units.sd <- aggregate(data[3:length(data)], list(data$units), sd, na.rm = T)
colnames(table.samples) <- c("Sample", "IRSL/BOSL1s", "IRSL cts", "BOSL1s", "fast","medium","slow", "OSL cts", "TL325oC","TL325 cts", "TL325pos","TL110oC", "TL110oC cts", "TL110pos")
colnames(table.samples.sd) <- c("Sample", "IRSL/BOSL1s.sd", "IRSL cts.sd", "BOSL1s.sd", "fast.sd","medium.sd","slow.sd","OSL cts.sd", "TL325oC.sd", "TL325 cts.sd", "TL325pos.sd","TL110oC.sd", "TL110oC cts.sd", "TL110pos.sd")
#colnames(table.units) <- c("Units", "IRSL/BOSL1s", "BOSL1s", "fast","medium","slow","tl325","tl325pos","Tl110oC","tl110pos")
#colnames(table.units.sd) <- c("Units", "IRSL/BOSL1s", "BOSL1s", "fast","medium","slow","tl325","tl325pos","Tl110oC","tl110pos")
res.sample <- data.frame(table.samples, table.samples.sd)
#res.units <- data.frame(table.units, table.units.sd)

setwd("C:/Users/iande/Documentos/proyectos/fernanda/")
#write.xlsx(data, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "by_aliquot", append = T)
#write.xlsx(res.sample, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "by_sample", append = T)
#write.xlsx(res.units, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "by_unit", append = T)

par(mfrow = c(2,2))
matplot(cbind(sens.irsl,sens.osl,sens.tl110,sens.tl325), pch = 16:19, col = 2:5, cex= 2)
legend("topright", legend = c("IRSL","OSL","TL110","TL325"), pch = 16:19, col = 2:5)
matplot(cbind(irsl.cts, osl.cts, tl110.cts, tl325.cts), pch = 16:19, col = 2:5, cex= 2, log = "y")
legend("topright", legend = c("IRSL","OSL","TL110","TL325"), pch = 16:19, col = 2:5)
matplot(cbind(fast.prop, med.prop, slow.prop), pch = 16:18, cex = 2, col = 2:4)
legend("topright", legend = c("Fast", "Medium", "Slow"), pch = 16:18, col = 2:4)
matplot(cbind(tl110pos,tl325pos), pch = 16:17, cex = 2, col = 2:3)
legend("topright", legend = c("TL110pos", "TL325pos"), pch = 16:18, col = 2:4)