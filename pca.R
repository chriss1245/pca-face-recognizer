# Function that implements pca, returns, the mean used to scale, a vector with eigenvalues D and the matrix P which contains the eigenvectors (also known as the load)
pca <- function(X, center = T, scale = T){
    x_bar <- rep(0, dim(X)[2])
    X <- scale(X, center, scale)

    # Computing the covariance/correlation matrix
    Sigma <- t(X)%*%X
    
    # Getting the eigenvectors and eigenvalues
    eigen <- eigen(Sigma)
    
    return(list(mean = x_bar, D = eigen$values, P = eigen$vector))
    
}

# reading the images using pca for reducing its rgb to one dimension and saving it into a matrix
library(reshape2)
library(OpenImageR)

# Allocating the space needed
images <- array(0, dim = c(150, 36000))

# Getting the names of the images
paths <- list.files('data')

# Since we have the images inside the folder data, we add the data directory to the path in order to make it work
paths <- paste('data',paths, sep = '/')

# Reading, processng and saving the images
for(i in 1:length(paths)){
    # Reading
    im_rgb <- melt(readImage(paths[i]))
    #Decomposing into an rgb matrix
    im_rgb <- reshape(im_rgb, timevar = 'Var3', idvar = c('Var1', 'Var2'), direction = 'wide')
    im_rgb <- im_rgb[, substr(colnames(im_rgb), 1, 5) == 'value']
    im_rgb <- as.matrix(im_rgb)
    
    #applying pca
    prc <- pca(im_rgb, scale = T)
    
    # using just the first pca
    rotated <- im_rgb%*%prc$P[,1] 
    
    # saving the image into the matrix images
    images[i,]<- as.vector(rotated)
}

# Giving a label of a person to each image
labels = rep(1:25,6) # 25 people with 6 images each one
labels = sort(labels, method = 'radix') # sorting the labels
images <- cbind.data.frame(label = labels, images) # creating a data frame with the labels and the images


# Applying knn
library(class)
partition <- seq(1,150, 3) # I am using 2 images for testing for each person

fit <- knn(images[-partition,-1], images[partition,-1],images[-partition,1] , k = 1 )

# Confusion matrix
t <- table(fit, images[partition, 1])

#Accuracy
sum(diag(t))/length(partition)

