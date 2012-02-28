#PCA requires normalized and centered data - i.e. all series should share common dimension
#centered means "without mean", normalized - divided by sd()

dataset$CH <- (dataset$CH - mean(dataset$CH))/sd(dataset$CH)
dataset$HK <- (dataset$HK - mean(dataset$HK))/sd(dataset$HK)
dataset$SI <- (dataset$SI - mean(dataset$SI))/sd(dataset$SI)
dataset$RU <- (dataset$RU - mean(dataset$RU))/sd(dataset$RU)
dataset$EN <- (dataset$EN - mean(dataset$EN))/sd(dataset$EN)
dataset$US <- (dataset$US - mean(dataset$US))/sd(dataset$US)
dataset$JP <- (dataset$JP - mean(dataset$JP))/sd(dataset$JP)

pca <- prcomp(dataset);

pcacris <- prcomp(dataset[1543:1911])
pcapre <- prcomp(dataset[1:1542])
pcapost <- prcomp(dataset[1912:2624])


PC1 <- dataset$CH*pca$rotation['CH',1] + dataset$HK*pca$rotation['HK',1] + dataset$SI*pca$rotation['SI',1] + dataset$RU*pca$rotation['RU',1] + dataset$EN*pca$rotation['EN',1] + dataset$US*pca$rotation['US',1] + dataset$JP*pca$rotation['JP',1]
PC2 <- dataset$CH*pca$rotation['CH',2] + dataset$HK*pca$rotation['HK',2] + dataset$SI*pca$rotation['SI',2] + dataset$RU*pca$rotation['RU',2] + dataset$EN*pca$rotation['EN',2] + dataset$US*pca$rotation['US',2] + dataset$JP*pca$rotation['JP',2]
PC3 <- dataset$CH*pca$rotation['CH',3] + dataset$HK*pca$rotation['HK',3] + dataset$SI*pca$rotation['SI',3] + dataset$RU*pca$rotation['RU',3] + dataset$EN*pca$rotation['EN',3] + dataset$US*pca$rotation['US',3] + dataset$JP*pca$rotation['JP',3]
PC4 <- dataset$CH*pca$rotation['CH',4] + dataset$HK*pca$rotation['HK',4] + dataset$SI*pca$rotation['SI',4] + dataset$RU*pca$rotation['RU',4] + dataset$EN*pca$rotation['EN',4] + dataset$US*pca$rotation['US',4] + dataset$JP*pca$rotation['JP',4]








grangertest(US ~ PC2, data = PCdiff)
grangertest(PC2 ~ US, data = PCdiff)
