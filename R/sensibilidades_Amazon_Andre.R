library(ggplot2); library(xlsx); library(moments); 
library(ggpubr); library(gridExtra); library(grid)
setwd("C:/Users/iande/documents/proyectos/Parnaiba")
amaz.table.sample = read.xlsx("sensibilidades-Amazon-Compilado_IdR.xlsx", sheetName = "Geral")

#order by Unit/age
osl.table.sample$Unit = factor(osl.table.sample$Unit, levels = c("Serra Grande", "Pimenteiras", "Cabecas", "Longa", "Poti", "Piaui", "Motuca", "Sambaiba"))

#estimate average per Unit
amaz.table.mean = aggregate(amaz.table.sample[,c()], by = list(amaz.table.sample$Amostra), mean)
osl.table.mean = osl.table.mean[,-1]
colnames(osl.table.mean) = c("osl.mean", "fast.mean", "med.mean", "slow.mean","irsl.mean", "tl110.mean", "tl110pos.mean", "tl325pos.mean")
row.names(osl.table.mean) = c("Serra Grande", "Pimenteiras", "Cabecas", "Longa", "Poti", "Piaui", "Motuca", "Sambaiba")
osl.table.sd = aggregate(osl.table.sample[,c(1,4,7,10,13,16,19,22)], by = list(osl.table.sample$Unit), sd)
osl.table.sd = osl.table.sd[,-1]
colnames(osl.table.sd) = c("osl.sd", "fast.sd", "med.sd", "slow.sd", "irsl.sd", "tl110.sd", "tl110pos.sd", "tl325pos.sd")
n = aggregate(osl.table.sample$Unit, by = osl.table.sample["Unit"], length)
osl.table.se = as.data.frame(cbind(osl.table.mean$Unit, t(t(osl.table.sd[,1:8])/sqrt(n[,2]))))
colnames(osl.table.se) = c("osl.se", "fast.se", "med.se", "slow.se","irsl.se", "tl110.se", "tl110pos.se", "tl325pos.se")
osl.table.fm = data.frame(osl.table.mean, osl.table.sd[1:8], osl.table.se[1:8], Unit = c("Serra Grande", "Pimenteiras", "Cabecas", "Longa", "Poti", "Piaui", "Motuca", "Sambaiba"))
osl.table.fm$Unit = factor(osl.table.fm$Unit, levels = c("Serra Grande", "Pimenteiras", "Cabecas", "Longa", "Poti", "Piaui", "Motuca", "Sambaiba"))


#plots############################################################################
jpeg("BOSL vs IRSL cts samples.jpeg", width = 900, height = 900, quality = 100)
g1 = ggplot(osl.table.sample, aes(irsl, osl, group = Unit))+
  #geom_boxplot(width = 0.6,  outlier.size = 4, aes(shape = Unit, color = Unit))+
  geom_point(size = 3, aes(shape = Unit, color = Unit))+
  #geom_smooth(method = "lm", formula = osl.table.sample$tl325pos~osl.table.sample$osl)+
  scale_shape_manual(values = c(15:18,15:18))+
  scale_colour_manual(values = rainbow(8))+
  labs(x = expression("IRSL cts/1.2s"), y = expression("%BOSL"[F]))+
  #scale_y_continuous(limits = c(0,60))+
  #scale_y_continuous(trans = "log")+
  #scale_x_reverse(limits = c(450,200), n.breaks = 8)+
  #geom_errorbar(mapping = aes(Age, tl325pos, 
  #                            ymin = tl325pos-tl325pos.se, 
  #                            ymax = tl325pos+tl325pos.se),
  #              width = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 30, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 30),
        panel.border = element_rect(fill = NA))
g1
dev.off()

#remove outliers
#boxplot(osl.table.sample$osl, plot = F)
#out = boxplot(osl.table.sample$osl)$out
#osl.table.sample = osl.table.sample[-which(osl.table.sample$osl %in% out),]

jpeg("IRSL percentage.jpeg", width = 900, height = 800, quality = 100)
g2 = ggplot(osl.table.fm, aes(Unit, irsl.mean, group = Unit))+
  labs(x = expression("Stratigraphic unit"), y = expression("%IRSL"))+
  #scale_y_continuous(limits = c(0,55))+
  #scale_x_continuous(trans = "log")+
  #scale_x_reverse(limits = c(450,200), n.breaks = 8)+
  geom_errorbar(aes(Unit, irsl.mean, 
                    ymin = irsl.mean-irsl.se,
                    ymax = irsl.mean+irsl.se),
                width = 0)+
  #geom_errorbarh(aes(irsl.mean, osl.mean, 
  #                  xmin = irsl.mean-irsl.se,
  #                  xmax = irsl.mean+irsl.se),
  #              height = 0)+
  geom_point(size = 7, aes(shape = Unit, color = Unit))+
  scale_shape_manual(values = c(15:18,15:18))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 30, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.2, "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 30),
        panel.border = element_rect(fill = NA))
g2
dev.off()

get.legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

g.legend <- get.legend(g2)

jpeg("IRSL-BOSL vs IRSL.jpeg", width = 900, height = 900, quality = 100)
#grid.arrange(g1, g2+theme(legend.position = "none"), nrow = 1, ncol = 2)
grid.arrange(#g.legend, 
  g1+theme(legend.position = "none"), 
  g2+theme(legend.position = "none"), 
  heights = c(10), layout_matrix = rbind(c(2,3)),
  nrow = 1, ncol = 2)
dev.off()