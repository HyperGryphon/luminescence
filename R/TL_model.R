#simulate a TL curve########################################################
#Kitis et al (1998)

#model_tl = function(n.peaks, data){

#rm(list = ls())
#graphics.off()

#repeat {

#variables
kel = 273.15+25 #kelvin degrees for calculation
b1 = 1.5; b2 = 1.2; b3 = 2 #order of reaction, qtz = 1.00001, fdp = 2-4
E1  = seq(0.90,0.95,0.01); E2 = 1.33; E3 = 1.73 #activation energy eV
s1 = 10^11; s2 = 10^12; s3 = 10^13 #frequency factor 1/s
#s1 = 10^log10(sr1); s2 = 10^log10(sr2); s3 = 10^log10(sr3)
#sp1 = s1*nt1^(b1-1); sp2 = s2*nt2^(b2-1); sp3 = s3*nt3^(b3-1) #effective frequency factor (Pagonis et al., 2014)
#Nt1 = 10^12; Nt2 = 10^12; Nt3 = 10^12 #trap capacity m^3
nt1 = 2*10^4; nt2 = 2*10^5; nt3 = 10^5 #trap occupancy m^3
beta1 = 5; beta2 = 5; beta3 = 5 #heating rate K/s
k = 8.617333262145e-5 #boltzmann constant eV/K
Temp = kel+(0:450)

It1 = matrix(0,451,length(E1))
Ir1 = matrix(0,451,length(E1))

for (i in 1:length(E1)) {
#GOK Kitis et al 1998
It1[,i] = nt1*s1*exp(-(E1[i]/(k*Temp)))*(((((b1-1)*s1*k*Temp^2)/(beta1/E1[i]))*exp(-E1[i]/(k*Temp))*(1-(2*k*Temp)/E1[i]))+1)^(-b1/(b1-1))
It2 = nt2*s2*exp(-(E2/(k*Temp)))*(((((b2-1)*s2*k*Temp^2)/(beta2/E2))*exp(-E2/(k*Temp))*(1-(2*k*Temp)/E2))+1)^(-b2/(b2-1))
It3 = nt3*s3*exp(-(E3/(k*Temp)))*(((((b3-1)*s3*k*Temp^2)/(beta3/E3))*exp(-E3/(k*Temp))*(1-(2*k*Temp)/E3))+1)^(-b3/(b3-1))
}
for (i in 1:length(E1)) {
#FOK Kitis et al 1998
Ir1[,i] = nt1*s1*exp(-(E1[i]/(k*Temp)))*exp(-((s1*k*Temp^2)/(beta1/E1[i]))*exp(-E1[i]/(k*Temp))*(1-(2*k*Temp)/E1[i]))
Ir2 = nt2*s2*exp(-(E2/(k*Temp)))*exp(-((s2*k*Temp^2)/(beta2/E2))*exp(-E2/(k*Temp))*(1-(2*k*Temp)/E2))
Ir3 = nt3*s3*exp(-(E3/(k*Temp)))*exp(-((s3*k*Temp^2)/(beta3/E3))*exp(-E3/(k*Temp))*(1-(2*k*Temp)/E3))
}
#SOK Kitis et al 1998
#Ir1 = (nt1^2)*s1*exp(-(E1/(k*Temp)))*(((s1*k*Temp^2)/(beta1/E1))*exp(-E1/(k*Temp))*(1-(2*k*Temp)/E1)+1)^-2
#Ir2 = (nt2^2)*s2*exp(-(E2/(k*Temp)))*(((s2*k*Temp^2)/(beta2/E2))*exp(-E2/(k*Temp))*(1-(2*k*Temp)/E2)+1)^-2
#Ir3 = (nt3^2)*s3*exp(-(E3/(k*Temp)))*(((s3*k*Temp^2)/(beta3/E3))*exp(-E3/(k*Temp))*(1-(2*k*Temp)/E3)+1)^-2

#GOK if you have the intensity and position of the peak Pagonis et al 2014
Im1 = max(It1)
Tm1 = (which.max(It1)-1)+kel
Im2 = max(It2)
Tm2 = (which.max(It2)-1)+kel
Im3 = max(It3)
Tm3 = (which.max(It3)-1)+kel
#Ir1 = Im1*(b1^(b1/(b1-1)))*exp((E1/(k*Temp))*((Temp-Tm1)/Tm1))*((1+(b1-1)*((2*k*Tm1)/E1)+(b1-1)*(1-((2*k*Temp)/E1))*(Temp^2/Tm1^2)*exp((E1/(k*Temp))*((Temp-Tm1)/Tm1))))^(-b1/(b1-1))
sr1 = ((beta1-E1)/(k*Tm1^2))*((1+(b1-1)*((2*k*Tm1)/E1))^-1)*exp(E1/(k*Tm1))
#Ir2 = Im2*(b2^(b2/(b2-1)))*exp((E2/(k*Temp))*((Temp-Tm2)/Tm2))*((1+(b2-1)*((2*k*Tm2)/E2)+(b2-1)*(1-((2*k*Temp)/E2))*(Temp^2/Tm2^2)*exp((E2/(k*Temp))*((Temp-Tm2)/Tm2))))^(-b2/(b2-1))
sr2 = ((beta2-E2)/(k*Tm2^2))*((1+(b2-1)*((2*k*Tm2)/E2))^-1)*exp(E2/(k*Tm2))
#Ir3 = Im3*(b3^(b3/(b3-1)))*exp((E3/(k*Temp))*((Temp-Tm3)/Tm3))*((1+(b3-1)*((2*k*Tm3)/E3)+(b3-1)*(1-((2*k*Temp)/E3))*(Temp^2/Tm3^2)*exp((E3/(k*Temp))*((Temp-Tm3)/Tm3))))^(-b3/(b3-1))
sr3 = ((beta3-E3)/(k*Tm3^2))*((1+(b3-1)*((2*k*Tm3)/E3))^-1)*exp(E3/(k*Tm3))

#plot the convoluted TL curve adding the curves
#It1sum = 0:450; Ir1sum = 0:450
It1sum = rowSums(It1[,1:6])
Ir1sum = rowSums(Ir1[,1:6])
C = integer(451)#1e16*exp(-1.8/(k*Temp))+1000 #background
Temp = 0:450
totalt = rowSums(data.frame(It1[,1:length(E1)], It2, It3, C))
totalr = rowSums(data.frame(Ir1[,1:length(E1)], Ir2, Ir3, C))

#jpeg("TL325pos_model.jpg", width = 900, height = 700, quality = 100)
xlim = c(0,450); ylim = c(0,20000)#max(It$It1, It$It2, It$It3)+0.3*max(It$It1, It$It2, It$It3))
par(mar = c(4.5,4.8,2,1.5))
matplot(Temp, cbind(It1sum, It1[,1:length(E1)], It2, It3, totalt), 
        type = "l", col = c(9,8,8,8,8,8,8,7,6,1), lwd = c(3,3,3,3,3,3,3,3,3,2), lty = c(1,1,1,1,1,1,1,1,1,2), 
        xlab = "Temperature (ºC)", ylab = "TL intensity (a.u.)",
        cex.lab = 1.5, cex.axis = 1.3, xaxs = "i", yaxs = "i", #xaxt = "n", yaxt = "n",
        xlim = xlim, ylim = ylim)
#plot legend with maxImum TL peak position in ºC
legend(legend = c(paste0("TL max @ ", (which.max(It1)), " ºC, Ea1 = ", round(E1,2), " eV"),
                  paste0("TL max @ ", (which.max(It2)), " ºC, Ea2 = ", round(E2,2), " eV"),
                  paste0("TL max @ ", (which.max(It3)), " ºC, Ea3 = ", round(E3,2), " eV")),
                  #paste("TL total max @", (which.max(total)-1), "ºC")), 
       lty = 1, lwd = 3, col = c(4,3,2,1), ncol = 1,
       "topleft", cex = 1.3, title = paste0("GOK, b = ", round(b1,2)), bty = "n")
#dev.off()
par(new = F)
par(mar = c(4.5,4.8,2,1.5))
matplot(Temp, cbind(Ir1sum, Ir1[,1:length(E1)], Ir2, Ir3, totalr), 
        type = "l", col = c(5,4,4,4,4,4,4,3,2,1), lwd = c(3,3,3,3,3,3,3,3,3,2), lty = c(1,1,1,1,1,1,1,1,1,2), 
        xlab = "", ylab = "",
        cex.lab = 1.5, cex.axis = 1.3, xaxs = "i", yaxs = "i", #xaxt = "n", yaxt = "n",
        xlim = xlim, ylim = ylim)
#plot legend with maxImum TL peak position in ºC
legend(legend = c(paste0("TL max @ ", (which.max(Ir1)), " ºC, Ea1 = ", round(E1,2), " eV"),
                  paste0("TL max @ ", (which.max(Ir2)), " ºC, Ea2 = ", round(E2,2), " eV"),
                  paste0("TL max @ ", (which.max(Ir3)), " ºC, Ea3 = ", round(E3,2), " eV")),
       #paste("TL total max @", (which.max(total)-1), "ºC")), 
       lty = 1, lwd = 3, col = c(8,6,5,1), ncol = 1,
       "topright", cex = 1.3, title = "FOK", bty = "n")
#}
#}

#fit model to a dataset from tgcd###################################################
library(tgcd); library(Luminescence)
setwd("C:/Users/iande/Documentos/proyectos/thermochron/binfiles/")
sample = "TL-Front_Fk_L1444-1482-83-84_R2_201126.binx"
sample = read_BIN2R(sample)

#run in the binx file
for (aliquot in 5) {
  runtl = 4; #runbg = 9
  tl = subset(sample, sample@METADATA$RUN == runtl)
  tl = data.frame(tl@DATA)
  t = seq(1,450,1.8)+273.15+25
  plot(t,tl[,aliquot])
  inisBG = c(1,10,100)
  deconv = tgcd(as.matrix(data.frame(t,tl[aliquot])), npeak = 4, model = "g1", edit.inis = F, 
                subBG = T, inisBG = inisBG, nstart = 100)
  print(deconv$pars); print(deconv$FOM)#; deconv$sp
}

Im = 1:length(tl); Tm = 1:length(tl)
E = 1; b = 1.00001
for (i in 1:length(tl)) {
  Im[i] = max(tl[,i])
  Tm[i] = which.max(tl[,i])*1.8+273.15
}

inisPAR = cbind(Im, E = rep(E,length(tl)), Tm, b = rep(b, length(tl)))
npeak = 2 #if npeak >1, add more as.matrix(inisPAR[i,1:4]) to inisPAR
E = matrix(1,length(tl),1); b = matrix(1,length(tl),1); sf = matrix(1,length(tl),1); fom = matrix(1,length(tl),1)
for (i in 1:length(tl)) {
  deconv = tgcd(as.matrix(data.frame(t,tl[i])), npeak = npeak, model = "g2", edit.inis = F, 
                #inisPAR = t(cbind(as.matrix(inisPAR[i,1:4]), as.matrix(inisPAR[i,1:4]))), 
                subBG = T, inisBG = inisBG)
  E[i] = deconv$pars[2]
  b[i] = deconv$pars[4]
  sf[i] = deconv$sp[7]
  fom[i] = deconv$FOM[1]
}
deconv.res = data.frame(E,b,sf,fom); View(deconv.res)

#simulate########################################################
temps = seq(0+273.15, 450+273.15, 1)
simPeak(temps, n0 = 1e5, Nn = 1e10, 
        bv = 1.9999, ff = 1e19, ae = 1.1, hr = 5, typ = "g")
simqOTOR(temps, n0 = 1e5, Nn = 1e10, Ah = 1e-5, An = 1e-2,
         ff = 1e19, ae = 1.1, hr = 5)
