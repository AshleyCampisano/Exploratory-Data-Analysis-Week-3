#Exploratory Data Analysis - Week 3

#Hierarchical Clustering

set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = .2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = .2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x +.05, y +.05, labels = as.character(1:12))

dateFrame <- data.frame(x = x, y = y)
dist(dateFrame)
#matrix of points, pairwise distance

#h clust

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

#pretty dendograms
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1,length(hclust$labels)), hang = 0.1, ...){
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang, ...)
  text(x = x, y = y[hclust$order] - (max(hclust$height)* hang), labels = lab[hclust$order],
       col = lab.col[hclust$order], srt = 90, adj = c(1, .5), xpd = NA, ...)
}


#heat map

dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

#k means clustering

set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each =4), sd = .2)
y <- rnorm(12, mean = rep(c(1,2,1), each =4), sd = .2)
plot(x, y, col = "blue", pch = 19, cex =2)
text(x + .05, y + .05, labels = as.character(1:12))

# k means clustering

dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)

kmeansObj$cluster

par(mar = rep(.2,4))
plot(x,y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, lwd = 3)

#heatmaps

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1,2), mar = c(2,4,.1,.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

#dimension reduction - PCA and Singular Value Decomposition

#matrix data
set.seed(1234)
par(mar = rep(.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

#cluster the data
par(mar = rep(.2,4))
heatmap(dataMatrix)

#what if we add a pattern?

set.seed(678910)
for (i in 1:40) {
  #flip a coin
  coinFlip <- rbinom(1, size =1, prob = .5)
  # if coin is heads up, add a common pattern to that row
  if (coinFlip) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,3), each = 5)
  }
}

par(mar = rep(.2,4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(.2,4))
heatmap(dataMatrix)

#patterns in rows and columns

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)


#PCA - components of SVD (u and v)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector",
     pch = 19)
plot(svd1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)


#components of the SVD - variance explained

par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

#relationship of singular values to PC

svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[,1], svd1$v[,1], pch = 19, xlab = "PC 1", ylab = "Right Singular Vector 1")
abline(c(0,1))

#components of the SVD - variance explained

constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1), each =5)}
svd1 <- svd(constantMatrix)
par(mfrow = c(1,3))
image(t(constantMatrix)[, nrow(constantMatrix):1])
plot(svd1$d, xlab = "Column", ylab = "Singular Value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop of Variance Explained", pch = 19)

#adding a second pattern

set.seed(678910)
for (i in 1:40) {
  #flip a coin
  coinFlip1 <- rbinom(1, size = 1, prob = .5)
  coinFlip2 <- rbinom(1, size = 1, prob = .5)
  #if coin is heads add a common pattern to that row
  if (coinFlip1) {
    dataMatrix[i, ] <- dataMatrix[i,] + rep(c(0,5), each = 5)
  }
  if (coinFlip2) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,5),5)
  }
}

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

#svd - true patterns

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered): 1])
plot(rep(c(0,1), each =5),pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0,1),5), pch = 19, xlab = "Column", ylab = "Pattern 2")


svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[,2], pch = 19, xlab = "Column", ylab = "Second right singular vector")

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,2))
plot(svd1$d, pch = 19)
plot(svd1$d^2/sum(svd1$d^2), pch = 19)

# issues with missing values in PCA

dataMatrix2 <- dataMatrixOrdered
##randomly insert some missing data
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))

#plotting and color in R
library(grDevices)
pal <- colorRamp(c("red", "blue"))

pal(0)
pal(1)
pal(.5)

pal(seq(0,1,len = 10))

pal <- colorRampPalette(c("red", "yellow"))
pal(2)
pal(10)

#color brewer

library(RColorBrewer)

cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

#smoothscatter function

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x,y)








