rm(list = ls())
graphics.off()

#LDA###############################################################
library(MASS); library(tidyverse); library(caret); library(klaR); library(mda)
library(xlsx)

setwd("G:/Mi unidad/Parnaiba")
osl.table.sample = read.xlsx("supplementary material 1_luminescence results.xlsx", sheetName = "by_sample")
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

#Discriminant Analysis############################################################
# 1"osl","2fast",3"med",4"slow",5"tl110",6"tl110pos",7"tl325pos",8"irsl"
df = dfsamples[,c(1,7,10)]
set.seed(123)
training.samples = df$Group %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data = df[training.samples, ]
test.data = df[-training.samples, ]
preproc.param = train.data %>% 
  preProcess(method = c("center", "scale"))
train.transformed = preproc.param %>% predict(train.data)
test.transformed = preproc.param %>% predict(test.data)
#lda, mda, fda, qda, rda
lda.res = lda(Group~., data = train.transformed)
lda.res
predictions = lda.res %>% predict(test.transformed)
predictions
mean(predictions$class==test.transformed$Group)
lda.data = cbind(train.transformed, predict(lda.res)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(size = 3, aes(color = Group, shape = Group))+
  scale_shape_manual(values = c(15:18,15:18))+
  scale_colour_manual(values = rainbow(8))
