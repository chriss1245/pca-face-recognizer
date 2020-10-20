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


# Partitioning the data
partition <- seq(1,150, 6) # I am using 2 images for testing for each person
train_set <- images[-partition,-1]
train_labels <- images[-partition,1]
test_set <- images[partition,-1]
test_labels <- images[partition,1]

# Applying knn
pred <- tunedKNN(train_set, train_labels, test_set, k = 1, dist = T )

# Mean distance
pred[,2]

#Accuracy
sum(pred[,1] == test_labels)length(test_labels)


# Creating a knn from scratch, added more methods of computing the distance, and the option of getting the mean distance of a label
tunedKNN <- function(x, y, t, k = 1, method = 'euclidean', dist = F){ 
    # x= matrix with the vectors of training
    #y = labels of the train vectors
    #t = matrix of the vectors that are being classified
    #method = the kind of distance we are computing (euclidean, angle_based, mahalanobis)
    #dist = Indicating T if we want to get the mean distance of each classification
    
    
    # Calculates the distance between the vector that is being predicted and the vectors of the train matrix
    distance <- function(v){ # 
        d <- rep(Inf, length(x[,1]))
        if(method == 'euclidean'){
            for(i in 1:length(x[,1])){ # for each vector in the matrix x
                d[i] <- sqrt(sum((v-x[i,])^2))
            }
        }
        
        if(method == 'angle_based'){
            for(i in 1:length(x[,1])){
                d[i] <- sum(v*x[i,])*sum(v*v)*sum(x[i,]*x[i,])
            }
        }
        
        if(method == 'mahalanobis'){
            for(i in 1:length(x[,1])){
                d[i] <- sum(abs(v-x[i,]))
            }
        }
        
        return(d)
    }
    
    # Generates the label predicted
    label <- function(d){
        names(d) <- y   # assign the corresponding label to each distance
        d <- sort(d) # sort the distance vector keeping their labels
        d <- d[1:k] # keeps only the k minimum numbers
        
        options <- unique(names(d)) # takes the possible labels for the vector that is being predicted
        
        votes <- cbind(options, rep(0, length(options))) # allocates the space to save the count of labels
        for(i in 1:length(votes[,1])){ # count the labels
            votes[i,2] <- sum(names(d) == votes[i,1])
        }
        
        idx <- which(votes[,2] == max(votes[,2])) # takes the index of the winner label
        label <- votes[idx,1]
        mean_dist <- mean(d[which(names(d) == label)])
        
        if(length(idx) > 1){ #If there is not a clear winner we label it as Inf
            label = Inf
            mean_dist <- mean(d)
        }
        
        return(c(label, mean_dist))
    }
    
    # Transform the labels into numbers (and keeps the distance if dist equal true) if needed 
    cast <- function(){
        if(class(y) == "numeric"){
            labels[,1] <- as.numeric(as.character(labels[,1]))
        }
        labels[,2] <- round(as.numeric((as.character(labels[,2]))),6)
        if(!dist){
            labels <- labels[,-2]
        }
        return(labels)
    }
    
    
    ### Main program
    # Iterates distance and label for each vector that is being predicted, and returns the values casted if needed
    
    x <- as.matrix(x)
    y <- as.vector(y)
    t <- as.matrix(t)
    labels <- NULL
    for(i in 1:length(t[,1])){
            
        d <- distance(v=t[i,])
        labels <- rbind.data.frame(labels,c(label(d)))
            
    }
        
    return(cast())
       
}




