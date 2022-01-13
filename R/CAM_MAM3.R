rm(list = ls())
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones")
#cargar los datos
sample = "L0534"
data.gy = read.table(file = paste(sample,".txt", sep = ""), header = T, fill = T, sep = "\t")

#convertir los datos de segundos a grays
#data.gy = data.s * 0.109

#seleccionar solo las columnas de 50º y de 290º
data.gy50 = data.frame(data.gy$de50, data.gy$er50)
data.gy50 = na.omit(data.gy50)
data.gy290 = data.frame(data.gy$de290, data.gy$er290)
data.gy290 = na.omit(data.gy290)

#crear pdf con una concatenación del nombre del archivo, la terminación y la separación
#pdf(file = paste(sample, "_50.pdf", sep = ""))

#central age model
cam50 = calc_CentralDose(data.gy50,
                       plot = F)

#minimum age model
mam50 = calc_MinDose(data.gy50,
                   sigmab = 0.1,
                   plot = F)

# Show summary table that contains the most relevant results
mamres50 = get_RLum(mam50, "summary")
camres50 = get_RLum(cam50, "summary")

# Plot the dose distribution in an abanico plot and draw a line
# at the minimum dose estimate
plot_AbanicoPlot(data.gy50,
                 main = "3-parameter Minimum Age Model",
                 line = mamres50,
                 polygon.col = "none",
                 hist = T,
                 rug = T,
                 pch = 19,
                 summary = c("n", "mean", "mean.weighted", "median"),
                 line.col = "#cd2828",
                 grid.col = "none",
                 line.label = paste0(round(mamres50$de, 1), "\U00B1",
                                     round(mamres50$de_err, 1), " Gy"),
                 bw = 0.1,
                 summary.pos = "topleft",
                 mtext = bquote("Parameters: " ~
                                  sigma[b] == .(get_RLum(mam50, "args")$sigmab) ~ ", " ~
                                  gamma == .(round(log(mamres50$de), 1)) ~ ", " ~
                                  sigma == .(round(mamres50$sig, 1)) ~ ", " ~
                                  rho == .(round(mamres50$p0, 2))))

#plot radial
plot_RadialPlot(data.gy50,
                main = "Central Age Model",
                rug = T,
                mtext = paste("OD: ", paste0(round(cam50@data$summary$OD, 1), " %")),
                summary = c("n", "mean.weighted"),
                summary.pos = "topleft",
                pch = 19,
                grid.col = "none",
                line = camres50$de,
                line.col = "#cd2828",
                line.label = paste0(round(camres50$de, 1), "\U00B1",
                                    round(camres50$de_err, 1), " Gy"))

#plot kernel density
plot_KDE(data.gy50,
         main = "KDE",
         values.cumulative = T,
         boxplot = T,
         output = T,
         order = T,
         summary = c("n", "mean","se.abs", "sd.abs"),
         summary.pos = "topleft",
         summary.method = "weighted",
         bw = 20,
         pch = 19)

#añadir lineas al kde plot
abline(v = mamres50$de,
       col = "#387fda")
abline(v = camres50$de,
       col = "#cd2828")

#leyenda y texto de los age models
maminfo50 = paste("MAM3: ", paste0(round(mamres50$de, 1), "\U00B1",
                                 round(mamres50$de_err, 1), " Gy"))
caminfo50 = paste("CAM: ", paste0(round(camres50$de, 1), "\U00B1",
                                round(camres50$de_err, 1), " Gy"))

#subtitulo
mtext(text = paste(maminfo50),
      adj = 0,
      line = 0.5, 
      col = "#387fda")
mtext(text = paste(caminfo50),
      adj = 1,
      line = 0.5,
      col = "#cd2828")

#dev.off()

#pdf(file = paste(sample, "_290.pdf", sep = ""))

#central age model
cam290 = calc_CentralDose(data.gy290,
                       plot = F)

#minimum age model
mam290 = calc_MinDose(data.gy290, 
                    sigmab = 0.1,
                    plot = F)

# Show summary table that contains the most relevant results
mamres290 = get_RLum(mam290, "summary")
camres290 = get_RLum(cam290, "summary")

# Plot the dose distribution in an abanico plot and draw a line
# at the minimum dose estimate
plot_AbanicoPlot(data.gy290,
                 main = "3-parameter Minimum Age Model",
                 line = mamres290,
                 polygon.col = "none",
                 hist = T,
                 rug = T,
                 pch = 19,
                 summary = c("n", "mean", "mean.weighted", "median"),
                 line.col = "#cd2828",
                 grid.col = "none",
                 line.label = paste0(round(mamres290$de, 1), "\U00B1",
                                     round(mamres290$de_err, 1), " Gy"),
                 bw = 0.1,
                 summary.pos = "topleft",
                 mtext = bquote("Parameters: " ~
                                  sigma[b] == .(get_RLum(mam290, "args")$sigmab) ~ ", " ~
                                  gamma == .(round(log(mamres290$de), 1)) ~ ", " ~
                                  sigma == .(round(mamres290$sig, 1)) ~ ", " ~
                                  rho == .(round(mamres290$p0, 2))))

#plot radial
plot_RadialPlot(data.gy290,
                main = "Central Age Model",
                rug = T,
                pch = 19,
                mtext = paste("OD: ", paste0(round(cam290@data$summary$OD, 1), " %")),
                summary = c("n", "mean.weighted"),
                summary.pos = "topleft",
                grid.col = "none",
                line = camres290$de,
                line.col = "#cd2828",
                line.label = paste0(round(camres290$de, 1), "\U00B1",
                                    round(camres290$de_err, 1), " Gy"))

#plot kernel density
plot_KDE(data.gy290,
         main = "KDE",
         values.cumulative = T,
         boxplot = T,
         output = T,
         order = T,
         summary = c("n", "mean","se.abs", "sd.abs"),
         summary.pos = "topleft",
         summary.method = "weighted",
         bw = 40,
         pch = 19)

#añadir lineas al kde plot
abline(v = mamres290$de,
       col = "#387fda")
abline(v = camres290$de,
       col = "#cd2828")

#leyenda y texto de los age models
maminfo290 = paste("MAM3: ", paste0(round(mamres290$de, 1), "\U00B1",
                                 round(mamres290$de_err, 1), " Gy"))
caminfo290 = paste("CAM: ", paste0(round(camres290$de, 1), "\U00B1",
                                 round(camres290$de_err, 1), " Gy"))

#subtitulo
mtext(text = paste(maminfo290),
      adj = 0,
      line = 0.5, 
      col = "#387fda")
mtext(text = paste(caminfo290),
      adj = 1,
      line = 0.5,
      col = "#cd2828")

#dev.off()