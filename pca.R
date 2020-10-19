
pca <- function(X, center = T, scale = T){
    x_bar <- rep(0, dim(X)[2])
    X <- scale(X, center, scale)

    # Computing the covariance/correlation matrix
    Sigma <- t(X)%*%X
    
    # Getting the eigenvectors and eigenvalues
    eigen <- eigen(Sigma)
    
    return(list(mean = x_bar, D = eigen$values, P = eigen$vector))
    
}

library(reshape2)
library(OpenImageR)
images <- array(0, dim = c(150, 36000))
paths <- list.files('data')
paths <- paste('data',paths, sep = '/')
for(i in 1:length(paths)){
    im_rgb <- melt(readImage(paths[i]))
    im_rgb <- reshape(im_rgb, timevar = 'Var3', idvar = c('Var1', 'Var2'), direction = 'wide')
    im_rgb <- im_rgb[, substr(colnames(im_rgb), 1, 5) == 'value']
    im_rgb <- as.matrix(im_rgb)
    prc <- pca(im_rgb, scale = T)
    rotated <- im_rgb%*%prc$P[,1]
    images[i,]<- as.vector(rotated)
}

labels = rep(1:25,6)
labels = sort(labels, method = 'radix')
images <- cbind.data.frame(label = labels, images)
library(class)
partition <- seq(1,150, 3)

fit <- knn(images[-partition,-1], images[partition,-1],images[-partition,1] , k = 1 )

t <- table(fit, images[partition, 1])
sum(diag(t))/length(partition)

