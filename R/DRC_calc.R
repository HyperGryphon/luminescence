rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones")
sample = read.table("L0478_qtz.txt", sep = "\t", header = T)

dose = t(sample[seq(sample[1,2],7),2])

dnatural = mean(sample[1,7], sample[7,7], sample[14,7], sample[21,7],
                sample[28,7], sample[35,7], sampl[42,7], sample[49,7],
                sample[56,7], sample[63,7])
d1 = mean(sample[2,7], sample[8,7], sample[15,7], sample[22,7],
                sample[29,7], sample[37,7], sampl[43,7], sample[50,7],
                sample[57,7], sample[64,7])
d2 = mean(sample[3,7], sample[9,7], sample[16,7], sample[23,7],
                sample[30,7], sample[37,7], sampl[44,7], sample[51,7],
                sample[58,7], sample[65,7])
d3 = mean(sample[4,7], sample[10,7], sample[17,7], sample[24,7],
                sample[31,7], sample[38,7], sampl[45,7], sample[52,7],
                sample[59,7], sample[66,7])
d4 = mean(sample[5,7], sample[11,7], sample[18,7], sample[25,7],
                sample[32,7], sample[39,7], sampl[46,7], sample[53,7],
                sample[60,7], sample[67,7])
rc = mean(sample[6,7], sample[12,7], sample[19,7], sample[26,7],
          sample[33,7], sample[40,7], sampl[47,7], sample[54,7],
          sample[61,7], sample[68,7])
ry = mean(sample[7,7], sample[13,7], sample[20,7], sample[27,7],
          sample[34,7], sample[40,7], sampl[47,7], sample[55,7],
          sample[61,7], sample[68,7])

matplot(dose, 
        cbind(dnatural,d1,d2,d3,d4, rc, ry),
        pch = c(0, 19, 19, 19, 19, 2, 3),
        ylab = "lxtx",
        xlab = "Dose (Gy)")

lxtx = t(data.frame(d1, d2, d3, d4))
dose = c(700,1400,2800,5600)

expfit = nls(lxtx~(dose^b), start = c(b = 2), trace = T)
lines(dose, dose^coef(expfit))
