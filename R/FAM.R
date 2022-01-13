rm(list = ls())
setwd("C:/Users/iande/Documents/Geologia/dataciones/luminiscencia/Mejillones")
#cargar los datos
sample = "L0530"
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

#calcular FAM
fam50 = calc_FiniteMixture(data.gy50,
                           sigmab = 0.1,
                           n.components = 2,
                           plot.proportions = T,
                           plot = T)

# Show summary table that contains the most relevant results
famres50 = get_RLum(fam50, "summary")

#plot kernel density
plot_KDE(data.gy50,
         main = "KDE",
         values.cumulative = T,
         boxplot = T,
         output = T,
         order = T,
         summary = c("n", "mean", "se.abs", "sd.abs"),
         summary.pos = "topleft",
         summary.method = "weighted",
         bw = 20,
         pch = 19)

#añadir lineas al kde plot
abline(v = famres50$de[1],
       col = "blue")
abline(v = famres50$de[2],
       col = "red")

#leyenda y texto de los age models
faminfo50c1 = paste("COMP 1: ", paste0(round(famres50$de[1], 1), "\U00B1",
                                  round(famres50$de_err[1], 1), " Gy"))
faminfo50c2 = paste("COMP 2: ", paste0(round(famres50$de[2], 1), "\U00B1",
                                  round(famres50$de_err[2], 1), " Gy"))

#subtitulo
mtext(text = paste(faminfo50c1),
      adj = 0,
      line = 0.5,
      col = "blue",
      cex = 0.8)
mtext(text = paste(faminfo50c2),
      adj = 1,
      line = 0.5,
      col = "red",
      cex = 0.8)

#dev.off()

#pdf(file = paste(sample, "_290.pdf", sep = ""))

#calcular FAM
fam290 = calc_FiniteMixture(data.gy290,
                           sigmab = 0.1,
                           n.components = 2,
                           plot.proportions = T,
                           plot = T)

# Show summary table that contains the most relevant results
famres290 = get_RLum(fam290, "summary")

#plot kernel density
plot_KDE(data.gy290,
         main = "KDE",
         values.cumulative = T,
         boxplot = T,
         output = T,
         order = T,
         summary = c("n", "mean", "se.abs", "sd.abs"),
         summary.pos = "topleft",
         summary.method = "weighted",
         bw = 20,
         pch = 19)

#añadir lineas al kde plot
abline(v = famres290$de[1],
       col = "blue")
abline(v = famres290$de[2],
       col = "red")

#leyenda y texto de los age models
faminfo290c1 = paste("COMP 1: ", paste0(round(famres290$de[1], 1), "\U00B1",
                                  round(famres290$de_err[1], 1), " Gy"))
faminfo290c2 = paste("COMP 2: ", paste0(round(famres290$de[2], 1), "\U00B1",
                                     round(famres290$de_err[2], 1), " Gy"))

#subtitulo
mtext(text = paste(faminfo290c1),
      adj = 0,
      line = 0.5,
      col = "blue",
      cex = 0.8)
mtext(text = paste(faminfo290c2),
      adj = 1,
      line = 0.5,
      col = "red",
      cex = 0.8)

#dev.off()