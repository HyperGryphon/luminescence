library(xlsx)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Resultados/sec_bin/")
#cargar archivo .binx y leerlo
sample = "Dating_L0672_R1_160902.binx"

CW = read_BIN2R(paste(sample, sep = ""))
aliquot = 1
CW = Risoe.BINfileData2RLum.Analysis(CW, pos = aliquot)
CW = CW@records[[1]]@data
OSL = CW[,2]
t = CW[,1]
CW = data.frame(CW,t)
components = 3

#jpeg(paste("CW2LM", run, ".jpg", sep = "_"))
fit = fit_CWCurve(CW, n.components.max = components, fit.method = "LM",
            #log = "x",
            )
#dev.off()

write.xlsx(fit$component.contribution.matrix, "CW_decomp.xlsx")
write.xlsx(CW, "signal.xlsx")
sample = read.xlsx("CW_decomp.xlsx", col_names = T, sheetName = "Sheet1")
comp1 = sample[,10]*CW[,2]/100; comp2 = sample[,11]*CW[,2]/100; comp3 = sample[,12]*CW[,2]/100
par(mar = c(4.5,4.5,2,1.5))
matplot(t, cbind(comp1), type = "l", lwd = 2, lty = 1,
        cex = 1, cex.lab = 2, cex.axis = 2, xaxs = "i", yaxs = "i",
        col = c("#4467a4","#f49d36", "#e94e1c", "#52b79e"))