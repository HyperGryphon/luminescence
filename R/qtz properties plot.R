library(Luminescence)
library(readxl)
library(Hmisc)
library(reshape)
library(ggplot2)
library(cowplot)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados")
#cargar los datos
name = "L0680"
sample = read_excel("osl quartz properties.xlsx", sheet = name, col_names = T)

ir.dep.ratio = sample$`ir dep ratio`
ir.dep.err = ir.dep.ratio*0.1
ir.bl = sample$`irsl/blsl %`
recy = sample$recycling
recy.err = sample$recy.err
recu = sample$recuperation
recu.err = sample$recu.err
d0 = sample$d0
d0.err = sample$d0.err
de = sample$de
de.err = sample$de.err
pch = sample$pch2
xlim = c(0, 250)

df = data.frame(recy, recy.err, de, de.err, d0, d0.err, recy, recy.err, recu, recu.err, ir.dep.ratio)

p1 = ggplot(df, aes(x = de, y = recy))+
  geom_point(shape = 19, size = 2)+
  #geom_point(aes(x = de, y = ir.dep.ratio), shape = 17, size = 3, col = "red")+
  geom_errorbar(aes(ymax=recy+recy.err, ymin=recy-recy.err), width = 0.03*mean(de))+
  geom_errorbarh(aes(xmax = de+de.err, xmin = de-de.err, height = 0.1))+
  scale_x_continuous(limits = xlim, expand = c(0,0), position = "bottom")+
  scale_y_continuous(limits = c(0.5,1.5), expand = c(0,0), position = "left")+
  geom_abline(intercept = 1.1, slope = 0, linetype = "dashed", size = 0.5)+
  geom_abline(intercept = 1, slope = 0, linetype = "solid", size = 0.5)+
  geom_abline(intercept = 0.9, slope = 0, linetype = "dashed", size = 0.5)+
  #geom_vline(xintercept = 1, linetype = "solid", size = 0.5)+
  #geom_vline(xintercept = 0.9, linetype = "dashed", size = 0.5)+
  #geom_vline(xintercept = 1.1, linetype = "dashed", size = 0.5)+
  labs(x = expression(""),
       y = expression(""))+
  ggtitle(paste("Sample", name))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"))
  

p2 = ggplot(df, aes(x = de, y = recu))+
  geom_point(shape = 19, size = 2)+
  geom_errorbar(aes(ymax=recu+recu.err, ymin=recu-recu.err), width = 0.03*mean(de))+
  geom_errorbarh(aes(xmax = de+de.err, xmin = de-de.err, height = 1))+
  scale_x_continuous(limits = xlim, expand = c(0,0), position = "bottom")+
  scale_y_continuous(limits = c(-10,11), expand = c(0,0), position = "left")+
  #geom_abline(intercept = 1.1, slope = 0, linetype = "dashed", size = 0.5)+
  geom_abline(intercept = 5, slope = 0, linetype = "solid", size = 0.5)+
  #geom_abline(intercept = 0.9, slope = 0, linetype = "dashed", size = 0.5)+
  #geom_vline(xintercept = 1, linetype = "solid", size = 0.5)+
  #geom_vline(xintercept = 0.9, linetype = "dashed", size = 0.5)+
  #geom_vline(xintercept = 1.1, linetype = "dashed", size = 0.5)+
  labs(x = expression(""),
       y = expression(""))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"))

p3 = ggplot(df, aes(x = de, y = ir.dep.ratio))+
  geom_point(shape = 19, size = 2)+
  geom_errorbar(aes(ymax=ir.dep.ratio+ir.dep.err, ymin=ir.dep.ratio-ir.dep.err), width = 0.03*mean(de))+
  geom_errorbarh(aes(xmax = de+de.err, xmin = de-de.err, height = 0.1))+
  scale_x_continuous(limits = xlim, expand = c(0,0), position = "bottom")+
  scale_y_continuous(limits = c(0.5,1.5), expand = c(0,0), position = "left")+
  geom_abline(intercept = 1.1, slope = 0, linetype = "dashed", size = 0.5)+
  geom_abline(intercept = 1, slope = 0, linetype = "solid", size = 0.5)+
  geom_abline(intercept = 0.9, slope = 0, linetype = "dashed", size = 0.5)+
  #geom_vline(xintercept = 1, linetype = "solid", size = 0.5)+
  #geom_vline(xintercept = 0.9, linetype = "dashed", size = 0.5)+
  #geom_vline(xintercept = 1.1, linetype = "dashed", size = 0.5)+
  labs(x = expression("Equivalent dose (Gy)"),
       y = expression(""))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"))

plot_grid(p1,p2,p3, ncol = 1, align = "v")