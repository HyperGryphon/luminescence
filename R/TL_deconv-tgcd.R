#fit model to a dataset from tgcd###################################################
library(tgcd); library(Luminescence)
setwd("C:/Users/iande/Documentos/proyectos/thermochron/binfiles/")
sample = "TL-Front_L1416-24-26_R2_210714.binx"
sample = read_BIN2R(sample)

#run in the binx file
#for (aliquot in 33) {
  runtl = 3; aliquot = 31 #runbg = 9
  tl = subset(sample, sample@METADATA$RUN == runtl & sample@METADATA$POSITION == aliquot)
  t = seq(1,450,1.8)+273.15+21
  tl = tl@DATA
  plot(t,tl[[1]])
  inisBG = c(1,10,100)
  deconv = tgcd(as.matrix(data.frame(t,tl[[1]])), npeak = 2, model = "lw", edit.inis = F, 
                subBG = F, inisBG = inisBG, nstart = 100)
  print(deconv$pars); print(deconv$FOM)#; deconv$sp
#}

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