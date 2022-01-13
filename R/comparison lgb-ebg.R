library(Luminescence)
library(readxl)
library(ggplot2)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados")
#cargar los datos
sample = read_excel("De dependence.xlsx", sheet = "osl-irsl", col_names = T)

denormal = sample$de_normal
denormal_err  = sample$err_normal
deebg = sample$de_ebg
deebg_err = sample$err_ebg
df = data.frame(denormal, deebg)

ggplot(df, aes(denormal, deebg))+
  geom_errorbar(aes(ymax=deebg+deebg_err, ymin=deebg-deebg_err),
                width = 2)+
  geom_errorbarh(aes(xmax = denormal+denormal_err, xmin = denormal-denormal_err,
                     height = 4))+
  #scale_x_continuous(limits = c(-1,33), expand = c(0,1), position = "bottom")+
  #scale_y_continuous(limits = c(-1,33), expand = c(0,1), position = "left")+
  geom_abline(intercept = 0, slope = 0.9, linetype = "dashed", size = 0.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "solid", size = 0.5)+
  geom_abline(intercept = 0, slope = 1.1, linetype = "dashed", size = 0.5)+
  labs(x = expression("LBG D"[e]*" (Gy)"),
       y = expression("EBG D"[e]*" (Gy)"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.25, "cm"))

lm=lm(denormal~deebg)
summary(lm)$r.squared
