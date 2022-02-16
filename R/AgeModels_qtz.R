library(Luminescence)
library(readxl)
library(moments)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documentos/proyectos/services/maria chile/")
#cargar los datos
name = "L1520"
sample = read_excel("pirir290_analyst.xlsx", sheet = name, col_names = T)
#sample = read.table("16211-3_HF2x_130312-TEMP165.asc", header = T, sep = ",")
par(mar = c(5,5,1.5,1.5))

De = as.numeric(sample$ED)#*0.088
De.Error = as.numeric(sample$ED_Err)#*0.088
d0 = as.numeric(sample$Param2)
recy = as.numeric(sample$RR1)
recu = as.numeric(sample$Recup1)
#ir.ratio = sample$`irsl/blsl %`
#ir.dep.ratio = sample$`ir dep ratio`
df1 = data.frame(De, De.Error, d0, recy, recu)
De.data = data.frame(de=na.omit(df1$De), de.err=na.omit(df1$De.Error))
#De.data = subset(De.data, recy<=1.11 & recy>=0.89 & recu<=5)

#De.data = De.data[which(De.data$na.omit.df1.De.<=25),]

#crear pdf con una concatenación del nombre del archivo, la terminación y la separación
#pdf(file = paste(sample, "_qtz.pdf", sep = ""))

#central age model
cam = calc_CentralDose(De.data,
                       plot = F, log = T)

aam = calc_AverageDose(De.data, 
                       sigma_m = 0.1,
                       plot = F)

#minimum age model
mam = calc_MinDose(De.data,
                   log = T,
                   sigmab = 0.2,
                   par = 3,
                   bootstrap = F,
                   plot = F)

#maximum age model
max = calc_MaxDose(De.data,
                   sigmab = 0.1,
                   plot = F)

# Show summary table that contains the most relevant results
mamres = get_RLum(mam, "summary")
camres = get_RLum(cam, "summary")
maxres = get_RLum(max, "summary")

# Plot the dose distribution in an abanico plot and draw a line
# at the minimum dose estimate
plot_AbanicoPlot(De.data,
                 main = paste("Sample", name,  "\U007C CAM: ", paste0(round(camres$de, 1), " \U00B1 ",
                                                                      round(camres$de_err, 1), " Gy")),
                               #c("3-parameter Minimum Age Model"),
                 cex = 0.8,
                 #line = mean(De.data$na.omit.df.De.),
                 z.0 = camres$de,
                 mtext = paste("n = ", nrow(De.data), "/", nrow(df1), "|", "OD = ", paste0(round(cam@data$summary$OD, 1), " %")),
                 polygon.col = "none",
                 log.z = T,
                 hist = F,
                 rug = F,
                 frame = 1,
                 pch = 19,
                 bw = 0.1,
                 #zlim = c(0.3, 40),
                 grid.col = "none")
                 #summary = c("skewness"),
                 #line.col = "#cd2828",
                 #line.label = paste0(round(camres$de, 1), "\U00B1",
                  #                   round(camres$de_err, 1), " Gy"),
                 #line = camres,
                 #summary.pos = "topleft")
                 #mtext = bquote("Parameters: " ~ sigma[b] == .(get_RLum(mam, "args")$sigmab) ~ ", " ~
                  #                gamma == .(round(log(mamres$de), 1)) ~ ", " ~
                   #              sigma == .(round(mamres$sig, 1)) ~ ", " ~
                    #            rho == .(round(mamres$p0, 2))))

#plot radial CAM
plot_RadialPlot(De.data,
                main = paste("Sample",name, "|", paste0("CAM = ", round(camres$de, 1), " \U00B1 ",
                                                        round(camres$de_err, 1), " Gy")),
                rug = F,
                mtext = paste("n/N =", nrow(De.data), "/", nrow(df1), "|", "OD =", paste0(round(cam@data$summary$OD, 1), " %")),
                cex = 1.2,
                #summary = c("n"),
                #summary.pos = "topleft",
                central.value = camres$de,
                centrality = camres$de,
                pch = 19,
                grid.col = "none",
                #zlim = c(0, 30),
                log.z = T,
                plot.ratio = 1)
                #line = camres$de,
                #line.col = "#cd2828",
                #line.label = paste0(round(camres$de, 1), "\U00B1",
                #                    round(camres$de_err, 1), " Gy"))

#plot radial MAX
plot_RadialPlot(De.data,
                main = c("Maximum Age Model"),
                rug = T,
                summary = c("n", "mean.weighted"),
                summary.pos = "topleft",
                pch = 19,
                grid.col = "none",
                #zlim = c(0, 30),
                #plot.ratio = 0.5,
                line = maxres$de,
                line.col = "#cd2828",
                line.label = paste0(round(maxres$de, 1), "\U00B1",
                                    round(maxres$de_err, 1), " Gy"))

#plot kernel density
plot_KDE(De.data, main = "", yaxt = "n", na.rm = T,
         values.cumulative = T,
         boxplot = F,
         order = T,
         rug = F,
         #summary = c("n"),
         summary.pos = "topleft",
         summary.method = "weighted",
         bw = max(na.omit(De))/10,
         xlab = "Dose (Gy)", ylab = "Density", cex = 1.5,
         pch = 19, lwd = 2,
         yaxs = "i")

#añadir lineas al kde plot
abline(v = mamres$de,
       col = "#387fda")
abline(v = camres$de,
       col = "black", lty = 2)

#leyenda y texto de los age models
maminfo = paste("MAM3: ", paste0(round(mamres$de, 2), " \U00B1 ",
                                 round(mamres$de_err, 2), " Gy"))
caminfo = paste("Sample", name, " \U007C  CAM: ", paste0(round(camres$de, 2), " \U00B1 ",
                                round(camres$de_err, 2), " Gy"))
aliquot.number = paste(" (n = ", nrow(De.data), " / ", nrow(df1), ")", sep = "")

#subtitulo
mtext(text = paste(maminfo),
      adj = 1,
      line = 0.5, 
      col = "#387fda")
mtext(text = paste(caminfo, aliquot.number),
      adj = 0,
      line = 0.5,
      col = "black")

#finite age model
fam = calc_FiniteMixture(De.data,
                         sigmab = 0.1,
                         n.components = 2,
                         plot.proportions = T,
                         plot = T)

#dev.off()
#data.frame(camres$OD, kurtosis(De.data$na.omit.df.De.), skewness(De.data$na.omit.df.De.))
camres$rel_OD
data.frame(mean(na.omit(df1$d0)), sd(na.omit(df1$d0)),
           mean(na.omit(df1$recy)), sd(na.omit(df1$recy)),
           mean(na.omit(df1$recu)), sd(na.omit(df1$recu)),
           mean(na.omit(df1$ir.dep.ratio)), sd(na.omit(df1$ir.dep.ratio)))