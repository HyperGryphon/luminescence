library(Luminescence)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Mititus/sent2ian_12-20-17/PIRIR_raw_data_files/daybreak-output")

samplew = "pIRIR05a.txt"
samplew = read_Daybreak2R(samplew, raw = T)
aliquot = 5

data = get_RLum(samplew, recordType = "IRSL")
data.ir = set_RLum(class = "RLum.Analysis", records = data, protocol = "pIRIR")
data.ir = convert_RLum2Risoe.BINfileData(data)
data.ir = Risoe.BINfileData2RLum.Analysis(data.ir)
#sample.ir = subset(sample, LTYPE == "IRSL")
#seleccionar los datos de la curva IR50
data50 = samplew.ir[[1]][[1]]@data

post.sample = get_RLum(object = samplew[[1]]["MaxTemp" == 205])
#post.sample = subset(sample, LTYPE == "OSL")

sar = analyse_SAR.CWOSL(data.ir, 
                        signal.integral.min = 1,
                        signal.integral.max = 4,
                        background.integral.min = 349,
                        background.integral.max = 399,
                        xlab = "Dose (Gy)",
                        ylab = "Lx/Tx",
                        plot = T)
