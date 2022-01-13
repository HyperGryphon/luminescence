rm(list = ls())
graphics.off()
#setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones/sec_bin")

#data = na.omit(read.table("output.txt", sep = "\t", header = F))
#data[data==0] = NA
#matplot(data[,1], cbind(data[,4], data[,9], data[,11], data[,13], data[,15]), type = "l", log = "y", ylim = c(0.1, 10000))

t = 1:40 #time (s)
P = 40 #total stimulation time (s)

#background
a = 3.00E+001

#n = initial trap charge population
n1 = 3.13E+001
n2 = 9.82E+001
n3 = 4.31E+002

#b = detrapping probability
b1 = 9.08E+000
b2 = 2.49E+000
b3 = 2.12E-001

#Continuous wave
cw1 = n1*b1*exp(-b1*t)
cw2 = n2*b2*exp(-b2*t)
cw3 = n3*b3*exp(-b3*t)

y = a+cw1+cw2+cw3

cw1[cw1 == 0] = NA
cw2[cw2 == 0] = NA
cw3[cw3 == 0] = NA
y[y == 0] = NA

matplot(t, cbind(y, cw1, cw2, cw3, a),
        type = "l",
        lwd = 2,
        lty = 1,
        col = c("black", "red", "blue", "gray", "green"),
        ylim = c(1, max(y+0.2*y)),
        log = "y",
        xlab = "time (s)",
        ylab = "CW-OSL (cts / 0.1 s)")

legend("topright",
       legend = c("OSL", "fast", "medium", "slow", "background"),
       text.col = c("black", "red", "blue", "gray", "green"))

#transform CW into LM
t = 1:40 #time (s)
P = 40 #total stimulation time (s)
u = sqrt(2*t*P)

#Linear modulated
lm1 = (n1*b1*(t/P))*exp((-b1*t^2)/(2*P))
lm2 = (n2*b2*(t/P))*exp((-b2*t^2)/(2*P))
lm3 = (n3*b3*(t/P))*exp((-b3*t^2)/(2*P))

f = lm1+lm2+lm3+a

matplot(t, cbind(f, lm1, lm2, lm3, a),
        type = "l",
        lwd = 2,
        pch = c(1, 2, 3, 5),
        lty = 1,
        #log = "x",
        #xlim = c(1,100),
        col = c("black", "red", "blue", "gray", "green"),
        xlab = "time (s)",
        ylab = "pseudo LM-OSL (cts / 0.1 s)")

legend("topright",
       legend = c("linear sum", "fast", "medium", "slow", "background"),
       text.col = c("black", "red", "blue", "gray", "green"))

