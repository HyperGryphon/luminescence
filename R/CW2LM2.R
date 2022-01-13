rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/proyectos/Atacama - Tesis/sec_bin/")
#cargar archivo .binx y leerlo
sample = "Dating_L0672_R1_160902.binx"

aliquot = read_BIN2R(paste(sample, sep = ""))
pos = 3
run = 4
components = 3
bg = read_BIN2R("LM_5000_L0672-673-680_R2_170531.binx")
bg = Risoe.BINfileData2RLum.Analysis(bg, pos = 17)
bg = data.frame(bg[[1]][,1], bg[[1]][,2])

#t = 1:40
#b1 = 0.427
#b2 = 0.0540
#b3 = 0.000834
#n1 = 291
#n2 = 366
#n3 = 49.5

#xm1 = sqrt(max(t)/b1)
#xm2 = sqrt(max(t)/b2)
#xm3 = sqrt(max(t)/b3)

#im1 = exp(-0.5)*(n1/xm1)
#im2 = exp(-0.5)*(n2/xm2)
#im3 = exp(-0.5)*(n3/xm3)

#stval = data.frame(im = c(im1, im2, im3), 
#                    xm = c(xm1, xm2, xm3))

#convertir .binx a RLum, indicar numero de alicuotas
object = Risoe.BINfileData2RLum.Analysis(aliquot, 
                                         pos = pos,
                                         run = run)

curve = get_RLum(object)
plot(curve, na.omit(log = "x"))
lm = CW2pLM(curve)
plot(lm, type = "p", pch = 21)
lmmax = max(lm@data[,2])+0.1*max(lm@data[,2])
lm = data.frame(lm@data[,1], lm@data[,2])
lm[lm==0] = NA

#jpeg(paste("CW2LM", run, ".jpg", sep = "_"))
fit_LMCurve(na.omit(lm), n.components = components, fit.method = "LM",
            #log = "x",
            LED.power = 40, LED.wavelength = 470,
            #values.bg = bg, bg.subtraction = "linear", plot.BG = T,
            input.dataType = "pLM", #start_values = stval,
            main = paste(sample, "run ", run), ylim = c(0, lmmax))
#dev.off()