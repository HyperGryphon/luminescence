library(Luminescence)
library(Hmisc)
library(gridExtra)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados")
#cargar los datos
sample = read.table("relacion isotopos_2.txt", header = T, sep = "\t")
#seleccionar las muestras a plotear
name = sample[c(4,6,7,8,10,11,13),1]
URa = sample[c(4,6,7,8,10,11,13),2]
URa.err = sample[c(4,6,7,8,10,11,13),3]
UPb = sample[c(4,6,7,8,10,11,13),4]
UPb.err = sample[c(4,6,7,8,10,11,13),5]
df = data.frame(name, URa, URa.err, UPb, UPb.err)

p1 = ggplot(df, aes(name, UPb, URa))+
  geom_errorbar(aes(ymax= UPb+UPb.err, ymin = UPb-UPb.err),
               width = 0.5)+
  #geom_errorbarh(aes(xmax = UPb+UPb.err, xmin = UPb-UPb.err, height = 0.05))+
  geom_point(aes(name, UPb), shape = 19, size = 3)+
  scale_x_discrete()+
  scale_y_continuous(limits = c(0.7,1.7), expand = c(0,0.04), position = "left")+
  geom_abline(intercept = 1.1, slope = 0, linetype = "dashed", size = 0.5)+
  geom_abline(intercept = 1, slope = 0, linetype = "solid", size = 0.5)+
  geom_abline(intercept = 0.9, slope = 0, linetype = "dashed", size = 0.5)+
  labs(x = expression(""),
       y = expression({}^238*"U/"*{}^214*"Pb"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        plot.margin = unit(c(0.5,1,1,1), "cm"))

p2 = ggplot(df, aes(name, UPb, URa))+
  geom_errorbar(aes(ymax= URa+URa.err, ymin = URa-URa.err),
                width = 0.5)+
  #geom_errorbarh(aes(xmax = UPb+UPb.err, xmin = UPb-UPb.err, height = 0.05))+
  geom_point(aes(name, URa), shape = 19, size = 3)+
  scale_x_discrete()+
  scale_y_continuous(limits = c(0.7,1.7), expand = c(0,0.04), position = "left")+
  geom_abline(intercept = 1.1, slope = 0, linetype = "dashed", size = 0.5)+
  geom_abline(intercept = 1, slope = 0, linetype = "solid", size = 0.5)+
  geom_abline(intercept = 0.9, slope = 0, linetype = "dashed", size = 0.5)+
  labs(x = expression("Sample"),
       y = expression({}^238*"U/"*{}^226*"Ra"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.margin = unit(c(-0.5,1,0.5,1), "cm"))

grid.arrange(p1,p2)