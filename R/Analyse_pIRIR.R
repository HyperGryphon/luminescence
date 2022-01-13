library(Luminescence)
library(readxl)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados/sec_bin")
#cargar los datos
sample = "Dating_pIR225_L0536_R2_170922.binx"
data = read_BIN2R(sample)


##(b) Transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(data, pos=1)
##(c) Grep curves
object <- get_RLum(object, record.id)
##(d) Define new sequence structure and set new RLum.Analysis object
sequence.structure <- c(1,2)
sequence.structure <- as.vector(sapply(seq(0,length(object)-1,by = 3),
                                       function(x){sequence.structure + x}))
object <- sapply(1:length(sequence.structure), function(x){
  object[[sequence.structure[x]]]
})
object <- set_RLum(class = "RLum.Analysis", records = object, protocol = "pIRIR")
##(2) Perform pIRIR analysis (for this example with quartz OSL data!)
## Note: output as single plots to avoid problems with this example
results <- analyse_pIRIRSequence(object,
                                 signal.integral.min = 1,
                                 signal.integral.max = 10,
                                 background.integral.min = 910,
                                 background.integral.max = 1000,
                                 fit.method = "EXP",
                                 sequence.structure = c("TL", "IR50", "pIRIR225", "pIRIR290"),
                                 main = "",
                                 plot.single = TRUE)

