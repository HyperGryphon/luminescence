library(Luminescence)
library(readxl)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/articulos/challenges and limitations of qtz osl dating in atacama")
#sample = "L0536 50"
#dose.rate = 2.488
#data = read_excel("tabla resumen pIRIR225.xlsx", sheet = "scf", col_names = T, skip = 0)

#linea = 
age_faded = 0.865
error_faded = 0.155
g.value = 1.3
error.g = 0.4
tc = 900
tc.g = tc
  
df = data.frame(age_faded, error_faded)
#n = 3
#tc = 50/225 = 1113; 225 = 1302; 50/290 = 1132; 290 = 1335

fad = (calc_FadingCorr(g_value = c(g.value, error.g),
                tc = tc, tc.g_value = tc.g,
                age.faded = c(age_faded, error_faded),
                n.MC = 1000))
hun = calc_Huntley2006(data = df,
                        rhop = c(2.92e-06, 4.93e-07),
                        ddot = c(3.7, 0.4),
                        readerDdot = c(0.1198,0.003))

#results = get_RLum(fad)