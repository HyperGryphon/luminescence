library(Luminescence)
library(readxl)
library(moments)
rm(list = ls())
graphics.off()
setwd("C:/Users/iande/Documents/dataciones/luminiscencia/Resultados")
#cargar los datos
name = "L0680 225"
sample = read_excel("aliquots fdp mititus y scf.xlsx", sheet = name, col_names = T)

De = sample$ED
De.Error = sample$ED_Err
d0 = sample$Param2
recy = sample$RR1
recu = sample$Recup1
df1 = data.frame(De, De.Error, d0, recy, recu)
#condicional para filtrar datos
df = subset(df1, recy<=1.1 & recy>=0.9 & recu<=5)
De.data = data.frame(na.omit(df$De), na.omit(df$De.Error))

#crear pdf con una concatenación del nombre del archivo, la terminación y la separación
#pdf(file = paste(sample, "_qtz.pdf", sep = ""))

#central age model
cam = calc_CentralDose(De.data,
                       plot = F)

#minimum age model
#en teoría sigmab debería ser la OD de CAM cam$summary$OD
mam = calc_MinDose(De.data,
                   sigmab = 0.1,
                   plot = F)

#maximum age model
max = calc_MaxDose(De.data,
                   sigmab = 0.5,
                   plot = F)

#finite age model
fam = calc_FiniteMixture(De.data,
                         sigmab = 0.1,
                         n.components = 2,
                         plot.proportions = T,
                         plot = T)

# Show summary table that contains the most relevant results
mamres = get_RLum(mam, "summary")
camres = get_RLum(cam, "summary")
maxres = get_RLum(max, "summary")

# Plot the dose distribution in an abanico plot and draw a line at the minimum dose estimate
plot_AbanicoPlot(De.data,
                 main = paste("Sample", name, paste0("\U00B0", "C |"), 
                              paste0(round(camres$de, 1), " \U00B1 ",
                                                                      round(camres$de_err, 1), " Gy")),
                 mtext = paste("n = ", nrow(De.data), "/24", "|", 
                               "OD = ", paste0(round(cam@data$summary$OD, 1), " %")),
                 #line = mean(De.data$na.omit.df.De.),
                 z.0 = camres$de,
                 polygon.col = "none",
                 cex = 0.8,
                 log.z = T,
                 hist = F,
                 rug = F,
                 pch = 19,
                 #summary = c("skewness"),
                 line.col = "#cd2828",
                 grid.col = "none",
                 bw = 0.1)
                 #summary.pos = "topleft")
                 
#line = camres,
#line.label = paste0(round(camres$de, 1), "\U00B1",
#                   round(camres$de_err, 1), " Gy"),                 
#bquote("Parameters: " ~
                  #                sigma[b] == .(get_RLum(mam, "args")$sigmab) ~ ", " ~
                  #                gamma == .(round(log(mamres$de), 1)) ~ ", " ~
                  #                sigma == .(round(mamres$sig, 1)) ~ ", " ~
                  #                rho == .(round(mamres$p0, 2))))

#plot radial CAM
#plot_RadialPlot(De.data,
                main = paste("Sample", name, paste0("\U00B0", "C |"), 
                             paste0("CAM = ", round(camres$de, 1), " \U00B1 ",
                                    round(camres$de_err, 1), " Gy")),
                mtext = paste("n/N =", nrow(De.data), "/", nrow(df1), "|", 
                              "OD =", paste0(round(cam@data$summary$OD, 1), " %")),
                central.value = camres$de,
                centrality = camres$de,
                #summary = c("n"),
                #summary.pos = "topleft",
                rug = F,
                pch = 19,
                grid.col = "none",
                #zlim = c(0, 200),
                log.z = T)
                #plot.ratio = 0.3,
                #line = camres$de,
                #line.col = "#cd2828",
                #line.label = paste0(round(camres$de, 1), "\U00B1",
                #                    round(camres$de_err, 1), " Gy"))

#plot radial MAX
#plot_RadialPlot(De.data,
                main = c("Maximum Age Model"),
                rug = T,
                summary = c("n"),
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
#kde = plot_KDE(De.data, main = "", yaxt = "n",
               values.cumulative = T,
               boxplot = F,
               output = T,
               order = T,
               rug = F,
               summary = c("n", "kurtosis", "skewness"),
               summary.pos = "topleft",
               bw = 20,
               pch = 19,
               yaxs = "i")

#añadir lineas al kde plot
#abline(v = mamres$de,
#       col = "red", lty = 2)
#abline(v = camres$de,
#       col = "black", lty = 2)

#leyenda y texto de los age models
maminfo = paste("MAM3: ", paste0(round(mamres$de, 1), " \U00B1 ",
                                 round(mamres$de_err, 1), " Gy"))
caminfo = paste("Sample", name,  "\U007C CAM = ", paste0(round(camres$de, 1), " \U00B1 ",
                                                        round(camres$de_err, 1), " Gy, "),
                paste0("OD = ", round(cam@data$summary$OD, 1), " %"),
                #"\U007C MAM3 = ", paste0(round(mamres$de, 1), " \U00B1 ",
                #                       round(mamres$de_err, 1), " Gy"),
                paste(" (n = ", nrow(df), " / ", nrow(df1), ")", sep = ""))

#subtitulo
#mtext(text = paste(maminfo),
      adj = 1,
      line = 0.5, 
      col = "red")
#mtext(text = paste(caminfo),
      adj = 0,
      line = 0.5,
      col = "black")

#dev.off()
data.frame(camres$OD, kurtosis(De.data$na.omit.df.De.), skewness(De.data$na.omit.df.De.))
data.frame(mean(na.omit(df$d0)), sd(na.omit(df$d0)),
           mean(na.omit(df$recy)), sd(na.omit(df$recy)),
           mean(na.omit(df$recu)), sd(na.omit(df$recu)))