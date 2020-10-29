###############################################################################
#REQUIRES THAT setup.R HAS BEEN EXECUTED


# This file was made for processing the images we are using to train and test and to save the original dimensions of the images
# It produces a matrix without principal components
#             a matrix with pc using 95 percent of the variance
#             a matrix with pc using 87 percent of the variance



library(OpenImageR)
dim.rgb <- c(36000, 3) # dim of the rgb matrix
dim.image <- c(200, 180, 3) # dim of the image


# Getting the names of the images
paths <- list.files('data')

# Since we have the images inside the folder data, we add the data directory to the path in order to make it work
paths <- paste('data',paths, sep = '/')


# Allocating the space for the images
images <- matrix(0,150,200*180*3)
images95 <- matrix(0, 150, 200*180*2)
images87 <- matrix(0, 150, 200*180*1)


for(i in 1:150){
rgb.vector <- readImage(paths[i])
rgb.vector <- as.numeric(as.vector(rgb.vector))

# Saving the original image
images[i,] <- rgb.vector 


dim(rgb.vector) <- dim.rgb # Building the rgb matrix

comps <- pca(rgb.vector) # Applying pca

# Saving the image with  95 percent of variance
pcaIm95<- rgb.vector%*%comps$P[,1:2] 
images95[i,] <- as.vector(pcaIm95) 

# Saving the image with 87 percent of variance
pcaIm87 <- rgb.vector%*%comps$P[,1]
images87[i,] <- as.vector(pcaIm87)

}

# Labeling the images
labels <-rep(1:25,6)
labels <- sort(labels)

images95 <- cbind(labels, images95)
images87 <- cbind(labels, images87)
images <- cbind(labels, images)

#saving the images
save(images, images95, images87, dim.rgb, dim.image ,file = 'images.RData')

rm(list = ls())
