
data <- read.csv("Wholesale customers data.csv")
str(data)
summary(data)

data[, 1:2] <- apply(data[, 1:2], 2, function(x) as.factor(x))
vars <- colnames(data)
## to one hot encode factor values and normalize numeric ones
# cat <- vars[sapply(data[, vars], class) %in% c("factor", "character")]
num <- vars[sapply(data[, vars], class) %in% c("numeric", "integer")]
# for (i in cat) {
#   dict <- unique(data[, i])
#   for(key in dict){
#     data[[paste0(i, "_", key)]] <- 1.0 * (data[, i] == key)
#  }
# }
# data[, num] <- apply(data[, num], 2, function(x) {
#   (x - min(x)) / (max(x) - min(x))
# })
# 
# data <- data[, -which(colnames(data) %in% cat)]
# cmatrix <- as.matrix(sapply(data[, num], as.numeric))
## scale numeric values
cmatrix <- scale(data[, num])
head(cmatrix)
## Hierarchical Clustering
d <- dist(cmatrix, method = "euclidean")
# library(e1071)
# d <- hamming.distance(cmatrix)
pfit <- hclust(d, method = "ward.D")
plot(pfit, labels = FALSE)

plot(pfit, labels = FALSE)
rect.hclust(pfit, k = 4)

hcl_groups <- factor(cutree(pfit, k = 4))
## visualize clusters in the data
library(ggplot2)
data$hcl <- hcl_groups
ggplot(data, aes(x = log(Grocery),
                 y = log(Milk),
                 color = hcl)) +
  geom_point(alpha = 0.7, 
             position = position_jitter(h = 0.5, w = 0.5)) + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "Grocery", y = "Milk") 

## Bootstrap Evaluation of Clusters
library(fpc)
k_estimated <- 4
cboot_hclust <- clusterboot(cmatrix, clustermethod = hclustCBI,
                            method = "ward.D", k = k_estimated,
                            count = FALSE)
hcl_cboot_groups <- cboot_hclust$result$partition
cboot_hclust$bootmean
cboot_hclust$bootbrd

## picking K for kmeans
clustering_ch <- kmeansruns(cmatrix, krange = 1:10, criterion = "ch")
clustering_ch$bestk

clustering_asw <- kmeansruns(cmatrix, krange = 1:10, criterion = "asw")
clustering_asw$bestk

## plot it
library(reshape2)
criteria <- data.frame(k = 1:10, ch = scale(clustering_ch$crit), 
                       asw = scale(clustering_asw$crit))
criteria <- melt(criteria, id.vars = c("k"), 
                 variable.name = "measure", 
                 value.name = "score")
ggplot(criteria, aes(x = k, y = score, col = measure)) + 
  geom_point(aes(shape = measure)) + 
  geom_line(aes(linetype = measure)) + 
  scale_x_continuous(breaks = 1:10, labels = 1:10) + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal()

## clusterboot() revisited with kmeans
kmeans_cboot <- clusterboot(cmatrix, clustermethod = kmeansCBI,
                            runs = 100, iter.max = 100,
                            krange = 3, seed = 13,
                            count = FALSE)
kmeans_cboot_groups <- kmeans_cboot$result$partition
kmeans_cboot$bootmean
kmeans_cboot$bootbrd

## Visualizing Clusters
## PCA done on dimensions you want to examine the clusters on
## those dimensions should be scaled 
pca <- prcomp(cmatrix)
visual <- predict(pca, newdata = cmatrix)[, 1:2]
visual <- cbind(as.data.frame(visual), 
                cluster = as.factor(kmeans_cboot_groups))
ggplot(visual, aes(x = PC1, y = PC2)) + 
  geom_point(aes(col = cluster), 
             # position = position_jitter(h = 0.5, w = 0.5), 
             alpha = 0.7) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

library(ggbiplot)
ggbiplot(pca, obs.scale = 1,
         ellipse = TRUE, circle = TRUE,
         groups = factor(kmeans_cboot_groups)
) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal()			
