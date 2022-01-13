#rm(list = ls())
#graphics.off()

R = 8.314
Ea = 151.46
D0.alfa = 10^7.7
D0 = D0.alfa*60*60*24*365*10^6
cool.rate = 1:120
T = 90
A = 55

tau = -((R*(T+273.15)^2)/((Ea*1000)*(-cool.rate)))
Tc = (Ea*1000)/(R*log(A*tau*D0))
Tc = Tc-273.15

plot(cool.rate,Tc, ylim = c(20,100))
