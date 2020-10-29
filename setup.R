"Debes cambiar las images87 por images, esta es la matriz que tiene las imagenes sin pca"

if (Sys.info()['sysname'] == "Windows"){
        setwd(dirname(rstudioapi::getSourceEditorContext()$path))

}else{
    setwd(system("pwd", intern = T))}

## Installs if needed the packages specified in requirements.txt
    #If there are libraries not added yet, please add them to the txt file
req <- read.delim('requirements.txt', header = F, sep = "\b")[,1]
for (i in req)
    if (!require(i, character.only = T))
        install.packages(i)
rm(i, req)

<<<<<<< HEAD

# a)
=======
#

load('images.RData') # The images we are using


#------------------------------------------------------------------------------
#a
>>>>>>> test2
pca <- function(X, center = T, scale = T){
    x_bar <- rep(0, dim(X)[2])
    X <- scale(X, center, scale)
    
    # Computing the covariance/correlation matrix
    Sigma <- t(X)%*%X
    
    # Getting the eigenvectors and eigenvalues
    eigen <- eigen(Sigma)
    
    return(list(mean = x_bar, D = eigen$values, P = eigen$vector))
    
}
<<<<<<< HEAD
=======


#-------------------------------------------------------------------------------
#b

classifier <- function(parameters, newdata){  # the parameters of knn and pca and the test image
    results <- NULL
    for(f in 1:length(newdata[,1])){
        
        # parse the images into rgb matrices
        #im_rgb <- melt(img)
        #im_rgb <- reshape(im_rgb, timevar = 'Var3', idvar = c('Var1', 'Var2'), direction = 'wide')
        #im_rgb <- im_rgb[, substr(colnames(im_rgb), 1, 5) == 'value']
        #im_rgb <- as.matrix(im_rgb)
        
        # get the pca
        #comps <- pca(im_rgb, parameters$center, parameters$scale)
        #if(pca.var == 87){
        #   t <- im_rgb%*%pcomps$P[,1]
        
        #} elif(pca.var == 95){
        #    t <- as.vector(im_rgb%*%pcomps$P[,1:2])
        #}
        
        
        test <- newdata[f,]
        # use knn
        
        dmatrix=dist(rbind(test,parameters$x), method = parameters$method, diag = TRUE, upper = TRUE)
        
        dmatrix=as.matrix(dmatrix)
        
        dmatrix=dmatrix[1,2:(nrow(parameters$x)+1)]
        
        ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)
        
        labels_sel <- parameters$y[ordenados$ix[1:parameters$k]]
        
        uniqv <- unique(labels_sel)
        
        votes <- tabulate(match(labels_sel, uniqv))
        
        uniqv <- uniqv[which(votes == max(votes))] 
        
        if(length(uniqv) == 1){
            distance <- mean(ordenados$x[which(labels_sel==uniqv)])
            
        } else{
            uniqv <- 0
            distance <- mean(ordenados$x)
        }
        
        if(parameters$threshold != FALSE){
            if(threshold < distance){
                uniqv <- 0
                
            }
        }
        
            results <- rbind(results,cbind(uniqv, distance))
            
    }
    return(results)
}







library(doParallel)

ncores = detectCores()
cl = makeCluster(ncores-1) 
registerDoParallel(cl)

predictions <-foreach(h=seq(1,150,6), '.combine' = rbind )%dopar%{
    library(foreach)
    interval <- h:(h+5)
    data.inv <-  images87[interval,] # data is going to be labeled as 0
    data <- images87[-interval,] 
    k_folds <- 4
    partitions <- sample(rep(1:k_folds, 144/k_folds), replace = F)
    
    temp = NULL
    temp <- foreach(i=1:k_folds, '.combine' = rbind)%do%{
        
        train.data <- data[partitions != i,-1]
        train.labels <- data[partitions !=i, 1]
    
        
        test.data <- rbind(data[partitions == i,-1],data.inv[,-1])
        test.labels <- c(data[partitions == i, 1], rep(0, length(data.inv[,1])))
    
    
    
    parameters = list(x = train.data, y = train.labels, k =1, method = 'euclidean', threshold = F, similarity = T, pca.var=87, scale = F, Center = T)
    
    
        
    
    #accuracy[i] <- sum(pred[,1]==test.labels)/length(pred[,1])
    
    
    cbind(test.labels, classifier(parameters, test.data))
    }
    
    temp
}

stopCluster(cl)

#predictions.87.euclidean <- predictions
#save(predictions.eucliden, file = 'images.RData')

#-------------------------------------------------------------------------------
#c
"
K AND DISTANCE METHOD

In order to determince which is the best k we just run the classifier using cross validation giving to k numbers
from 1 to 5 (without any threshold and without including data which should by classified as 0).
Quickly we saw that the k which gives the best result is k = 1 in all the distance methods. 
On the other hand we saw that the distance method
that maximazes the accuracy with k between 1 and 3 were euclidean and manhattan which are pretty close and we
are taking them into account for estimating the threshold. 


# k euclidean manhattan maximun 
  1       0.9     0.906   0.706 
  2       0.6     0.66    0.44
  3       0.64    0.55    0.446



VARIANCE OF PCA
We just run pca with the rgb matrix of a photo. (We roughly  tried to run pca with each color(3 matrices of 200 * 180)
and then joining them into a vector. And we got worse results. However, at that moment we had a buggy knn. If we have time
we would probably try again. If so. You woud not see this.) we got three eigenvectors. The first one contains 
around the 87 percent, the combinartion of the two first eigenvectors gave around the 97 percent.
(All the image proccessing is into another r file with a name related.)
Finally, by crossvalidation we got that the amount of variance retained should be #### in order to maximize the accuracy.

SIMILARITY METRIC
The similarity metric we are using is the distance. We saw that we could make a kind of formula to
roughly get the percentaje of similarity {(1/distance+1) wich is 1 if distance is 0, and aproaches 
to 0 as the dist is increasing. However it makes the same task than the distance. So we interpretated the distance as our
messumerment of similarity. Our threshold will be fixed using the distance. 

DISTANCE {our metric of similarity}
This distance can be calculated as the average of the distances of the k nearest neighbors which have the winner.
However since k = 1 it is only the distance from the clossest neighbor to the predicted data. 

THRESHOLD
The threshold is a tricky task. Our way of determinating it was through statistical inference. 
We decided to save the distance of a lot of predictions with our most succesful method for the distance.
Then we plot this data into histograms and see which is its distribution.
Then we got the confidence interval for that determinated distribution.
We know that the real distribution could be different than the one we got. 
But it is our most accurate approximation.




"


#-------------------------------------------------------------------------------
#d

#-------------------------------------------------------------------------------
#e



>>>>>>> test2
