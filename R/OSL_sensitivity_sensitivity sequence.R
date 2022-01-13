rm(list = ls())
graphics.off()

library(Luminescence); library(tibble); library(tidyverse); library(xlsx)
#chose directory
setwd("C:/Users/iande/Documentos/proyectos/fernanda/binfiles/")
#chose sample
sample <- "Sens_Polimin_155855-59-61-65-67-83-93-155909-19_R1_210224.binx"

#samples name and how many aliquots per sample
samples <- c(rep("155855", 5), rep("155859", 5), rep("155861", 5), rep("155865", 5),
            rep("155867", 5), rep("155883", 5), rep("155893", 5), rep("155909", 5),
            rep("155919", 5))
#units name and how many samples per unit
#units <- c(rep("Amazon", 8), rep("Xingu", 8))

#Parnaiba sensitivity sequence
runnat <- 1; runitl <- 3; runirsl <- 4; runosl <- 5; runtl325 <- 6; runtl110 <- 8; runtlbg <- 9

#select channels of integration and background for deconvolution
t <- seq(0.25,100,0.25)
ch <- 4; bg1 <- 361; bg2 <- 400

#how many components, max <- 4
components <- 3
#Do you want to plot the deconvolution graphs?
plot.deconv <- F

#How many degrees per channel for TL signals
degree <- 2

#integration intervals, check channels/s for sensitivity calculation
natint <- 1:4; natbg <- 361:400; nattot <- 1:400
itlint <- 1:10; itlbg <- 200:250; itltot <- 1:250
irslint <- 1:3; irslbg <- 721:750; irsltot <- 1:750
oslint <- 1:4; oslbg <- 361:400; osltot <- 1:400
tl325int <- (240/degree):(380/degree); tl325bg <- (240/degree):(380/degree); tl325tot <- 1:250
tl110int <- (70/degree):(170/degree); tl110bg <- (70/degree):(170/degree); tl110tot <- 1:250


########################################################################
################ just press ctrl+A and enter to run ####################
########################################################################

data <- read_BIN2R(paste(sample, sep = ""))
n.aliquots <- length(unique(data@METADATA$POSITION))
n.samples <- n.aliquots/9

#Select signals
nat <- subset(data, data@METADATA$RUN == runnat)
itl <- subset(data, data@METADATA$RUN == runitl)
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

nat <- data.frame(nat@DATA)
itl <- data.frame(itl@DATA)
irsl <- data.frame(irsl@DATA)
osl <- data.frame(osl@DATA)
tl325 <- data.frame(tl325@DATA)
tl110 <- data.frame(tl110@DATA)
tlbg <- data.frame(tlbg@DATA)

sens.nat <- 1:n.aliquots
for (i in 1:n.aliquots) {
  sens.nat[i] <- (sum(nat[,i][natint])-mean(nat[,i][natbg]*max(natint)))/(sum(nat[,i][nattot])-mean(nat[,i][natbg]*max(natbg)))*100
}

sens.itl <- 1:n.aliquots
for (i in 1:n.aliquots) {
  sens.itl[i] <- sum(itl[,i][itlint])-(sum(itl[,i][itlbg]))*0.2
}

osl.s <- 1:n.aliquots; bg.osl <- 1:n.aliquots
osl.total <- 1:n.aliquots; bg.total <- 1:n.aliquots
sens.osl <- 1:n.aliquots; osl.cts <- 1:n.aliquots
sens <- 1:n.aliquots; sd.bg.osl <- 1:n.aliquots
for (i in 1:n.aliquots) {
  
  osl.s[i] <- sum(osl[,i][1:ch])
  osl.total[i] <- sum(osl[,i][1:bg2])
  bg.osl[i] <- mean(osl[,i][bg1:bg2])*length(osl[,i][1:ch])
  sd.bg.osl[i] <- sd(osl[,i][bg1:bg2])
  bg.total[i] <- mean(osl[,i][bg1:bg2])*length(osl[,i][1:bg2])
  # OSL in cts/1s
  osl.cts[i] <- (osl.s[i]-bg.osl[i]) #/(doses[i]*8.9) #cts/Gy·mg
  # BOSLF
  sens.osl[i] <- (osl.s[i]-bg.osl[i])/(osl.total[i]-bg.total[i])*100
  
  if (sens.osl[i] < 0) {
    sens[i] <- 0
  } 
  else if (osl.cts[i] < (bg.osl[i]+3*sd.bg.osl[i])) {
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
  irsl.cts[i] <- sum(irsl[,i][irslint])-mean(irsl[,i][irslbg]*max(irslint))
  sens.irsl[i] <- (sum(irsl[,i][irslint])-mean(irsl[,i][irslbg]*max(irslint)))/(sum(osl[,i][oslint])-mean(osl[,i][oslbg]*max(oslint)))*100
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
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])/
                     (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    slow.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    slow.2.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])/
                        (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                           sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                           sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])+
                           sum(fit$component.contribution.matrix[[1]][,"cont.c4"][1:ch])))*100
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c3"])
    slow.2[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c4"])
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 12) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])/
                     (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])))*100
    slow.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c3"][1:ch])))*100
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c3"])
    slow.2[i] <- data.frame(print(NA))
  }
  if (ncol(fit$component.contribution.matrix[[1]]) == 9) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                         sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])))*100
    med.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])/
                     (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])+
                        sum(fit$component.contribution.matrix[[1]][,"cont.c2"][1:ch])))*100
    slow.prop[i] <- print(NA)
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c2"])
    slow[i] <- data.frame(print(NA))
    slow.2[i] <- data.frame(print(NA))
  }
  if(ncol(fit$component.contribution.matrix[[1]]) == 6) {
    fast.prop[i] <- (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])/
                      (sum(fit$component.contribution.matrix[[1]][,"cont.c1"][1:ch])))*100
    med.prop[i] <- print(NA)
    slow.prop[i] <- print(NA)
    slow.2.prop[i] <- print(NA)
    fast[i] <- data.frame(fit$component.contribution.matrix[[1]][,"cont.c1"])
    med[i] <- data.frame(print(NA))
    slow[i] <- data.frame(print(NA))
    slow.2[i] <- data.frame(print(NA))
  }
}

sens.tl325 <- 1:n.aliquots; tl325.cts <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl325.cts[i] <- sum(tl325[,i][tl325int])-sum(tlbg[,i][tl325bg])
  sens.tl325[i] <- ((sum(tl325[,i][tl325int])-sum(tlbg[,i][tl325bg]))/(sum(tl325[,i][tl325tot])-sum(tlbg[,i][tl325tot])))*100 
}

tl325pos <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl325pos[i] <- which.max(abs(tl325[,i][tl325int]))*degree-degree+tl325int[1]*degree-degree
}

sens.tl110 <- 1:n.aliquots; tl110.cts <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl110.cts[i] <- sum(tl110[,i][tl110int])-sum(tlbg[,i][tl110bg])
  sens.tl110[i] <- ((sum(tl110[,i][tl110int])-sum(tlbg[,i][tl110bg]))/(sum(tl110[,i][tl110tot])-sum(tlbg[,i][tl110tot])))*100 
}

tl110pos <- 1:n.aliquots
for (i in 1:n.aliquots) {
  tl110pos[i] <- which.max(abs(tl110[,i][tl110int]))*degree-degree+tl110int[1]*degree-degree
}

#condense data
data <- tibble(samples, sens.irsl, irsl.cts, sens.osl, fast.prop, med.prop, slow.prop, osl.cts, sens.tl325, tl325.cts, tl325pos, sens.tl110, tl110.cts, tl110pos)
table.samples <- aggregate(data[2:length(data)], list(data$samples), mean, na.rm = T)
table.samples <- tibble(table.samples)
table.samples.sd <- aggregate(data[2:length(data)], list(data$samples), sd, na.rm = T)
table.samples.sd <- tibble(table.samples.sd)
#table.units <- aggregate(data[3:length(data)], list(data$units), mean, na.rm <- T)
#table.units.sd <- aggregate(data[3:length(data)], list(data$units), sd, na.rm <- T)
colnames(table.samples) <- c("Sample", "IRSL/BOSL1s", "IRSL cts", "BOSL1s", "fast","medium","slow", "OSL cts", "TL325oC","TL325 cts", "TL325pos","TL110oC", "TL110oC cts", "TL110pos")
colnames(table.samples.sd) <- c("Sample", "IRSL/BOSL1s.sd", "IRSL cts.sd", "BOSL1s.sd", "fast.sd","medium.sd","slow.sd","OSL cts.sd", "TL325oC.sd", "TL325 cts.sd", "TL325pos.sd","TL110oC.sd", "TL110oC cts.sd", "TL110pos.sd")
#colnames(table.units) <- c("Units", "IRSL/BOSL1s", "BOSL1s", "fast","medium","slow","tl325","tl325pos","Tl110oC","tl110pos")
#colnames(table.units.sd) <- c("Units", "IRSL/BOSL1s", "BOSL1s", "fast","medium","slow","tl325","tl325pos","Tl110oC","tl110pos")
res.sample <- data.frame(table.samples, table.samples.sd)
#res.units <- data.frame(table.units, table.units.sd)

setwd("C:/Users/iande/Documentos/proyectos/fernanda/")
write.xlsx(data, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "by_aliquot", append = T)
write.xlsx(res.sample, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "by_sample", append = T)
#write.xlsx(res.units, file = paste0(str_sub(sample, 1, -6),".xlsx"), sheetName = "by_unit", append = T)
