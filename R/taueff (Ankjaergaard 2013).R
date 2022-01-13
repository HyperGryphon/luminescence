graphics.off()
rm(list = ls())

library(xlsx); library(ggplot2)

setwd("C:/Users/iande/Downloads/")
data = read.xlsx("data.xlsx", header = T, sheetIndex = 1)

time = data[,1]
I160 = data[,2]
I200 = data[,3]
I240 = data[,4]
I280 = data[,5]
I300 = data[,6]
I320 = data[,7]

t = c(160,200,240,280,300,320)+273
k = 8.617333262145*10^-5

c = 1.98

i1 = nls(I160~(1+((c*time)/taueff))^(-1/c), data = data.frame(I160,time),
        start = list(taueff = 10))
i1 = summary(i1)
i2 = nls(I200~(1+((c*time)/taueff))^(-1/c), data = data.frame(I200,time),
         start = list(taueff = 10))
i2 = summary(i2)
i3 = nls(I240~(1+((c*time)/taueff))^(-1/c), data = data.frame(I240,time),
         start = list(taueff = 10))
i3 = summary(i3)
i4 = nls(I280~(1+((c*time)/taueff))^(-1/c), data = data.frame(I280,time),
         start = list(taueff = 10))
i4 = summary(i4)
i5 = nls(I300~(1+((c*time)/taueff))^(-1/c), data = data.frame(I300,time),
         start = list(taueff = 10))
i5 = summary(i5)
i6 = nls(I320~(1+((c*time)/taueff))^(-1/c), data = data.frame(I320,time),
         start = list(taueff = 10))
i6 = summary(i6)

i = c(log10(i1$parameters[1]),log10(i2$parameters[1]),log10(i3$parameters[1]),
      log10(i4$parameters[1]),log10(i5$parameters[1]),log10(i6$parameters[1]))

x = (1/(k*t))
x = x[2:6]; i = i[2:6]
data = data.frame(i,x)
m = lm(x~i)

ggplot()+
  geom_point(data, mapping = aes(x, i), pch = 16, color = 1, size = 3)+
  #geom_errorbar(mapping = aes(x, i, 
  #                            ymin = lxtx-lxtx.err, 
  #                            ymax = lxtx+lxtx.err),width = 0.1)+
  geom_smooth(level = 0.68, aes(x, i), method = "lm")+
  #scale_y_continuous(limits = c(10,70))+
  labs(x = "1/(k*T)", y = expression("log(tau"[eff]*")"))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"))+
  annotate(geom = "text", x = min(x)+1, y = max(i), 
           label = paste0("E = ", round(m$coefficients[2],2), " eV",
                          "\n", 
                          "s = ", round(m$coefficients[1],2), "s^-1"),
           cex = 8)

summary(m)