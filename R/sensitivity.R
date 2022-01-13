rm(list = ls())
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones")
#cargar archivo .binx y leerlo
sample = "DoseRec2_pIR290_L0478_R2_160308"
aliquot = read_BIN2R(paste(sample, ".binx", sep = ""))

## convert Risoe.BINfileData object to RLum.Analysis object
data <- Risoe.BINfileData2RLum.Analysis(aliquot, pos = 1)
## extract all OSL curves
allCurves <- get_RLum(data)
## keep only the natural and regenerated signal curves
#pos <- seq(1, 9, 2)

# you may also use this function to check whether all
# TD curves follow the same shape (making it a TnTx(t) plot).
posTD <- seq(2, 10, 2)
sample <- allCurves[posTD]
plot_NRt(sample, main = "TnTx(t) Plot",
         smooth = "rmean", k = 50,
         ylab = "TD natural / TD regenerated",
         xlim = c(0, 200), legend = FALSE,
         pch = 19,
         legend.pos = "topleft")