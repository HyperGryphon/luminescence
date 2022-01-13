rm(list = ls())
graphics.off()

#PCA###############################################################
library(factoextra); library(cluster); library(FactoMineR); library(corrplot)
library(gplots); library(xlsx)

setwd("C:/Users/iande/documents/proyectos/Parnaiba")
osl.table.sample = read.xlsx("supplementary material 1_luminescence results.xlsx", sheetName = "by_sample")
rownames(osl.table.sample)  = osl.table.sample[,1]
osl.table.sample = osl.table.sample[,-1]
osl.table.mean = aggregate(osl.table.sample[,c("osl","fast","med","slow","tl110","tl110pos","tl325pos","irsl")], by = list(osl.table.sample$Unit), mean)

#prepare datasets
#original order is "osl","fast","med","slow","tl110","tl110pos","tl325pos","irsl"
using = c("osl","fast","med","slow","tl110","tl110pos","tl325pos","irsl")
dfsamples = as.data.frame(osl.table.sample[,c(using,"Unit","Group")])
dfunits = aggregate(osl.table.sample[,using], by = list(osl.table.sample$Unit), mean)
group = c("Serra Grande", rep("Caninde", 4), rep("Balsas",3))
dfunits$Group.1 = factor(dfunits$Group.1, levels = c("Serra Grande", "Pimenteiras", "Cabecas", "Longa", "Poti", "Piaui", "Motuca", "Sambaiba"))
dfunits = cbind(dfunits[,using], group)
colnames(dfunits) = c(using, "Group")
row.names(dfunits) = osl.table.mean[,1]

#cluster
df = scale(dfsamples[,c(-9,-10)])
fviz_nbclust(df, kmeans, method = "wss")
set.seed(123)
km.res = kmeans(df, 5)
fviz_cluster(km.res, data = df,
             palette = rainbow(8),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
#Principal component analysis for all samples
samples.pca = PCA(dfsamples[,c(-9,-10)], graph = T)
fviz_pca_biplot(samples.pca, col.ind = dfsamples[,10], 
                palette = rainbow(8), 
                pointshape = 21, fill.ind = dfsamples[,10],
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Units")
#Principal component analysis for units
res.pca = PCA(dfunits[,c(-9)], graph = T)
fviz_pca_biplot(res.pca, col.ind = dfunits[,9], 
                palette = "jco", 
                pointshape = 21, fill.ind = dfunits[,9],
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Units")
#Correspondence analysis for samples
res.ca = CA(dfsamples[,c(-9,-10)], ncp = 5, graph = TRUE)
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
fviz_ca_row(res.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
fviz_ca_biplot(res.ca,
               map ="symmetric", arrow = c(F, F),
               repel = TRUE)
#Correspondence analysis for units
df = as.table(as.matrix(dfunits))
balloonplot(t(df[,c(-9)]), main ="units", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
chisq.test(dfunits[,c(-9)])
