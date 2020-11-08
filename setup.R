
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



dim.rgb <- c(36000, 3)# dimenssion of the rgb matrix

load('rdata/images.RData') 
load('rdata/images95.RData')
load('rdata/images87.RData')
# a)

#




#---------------------------------AAAAAAAAAAAAAA---------------------------------------------

pca <- function(X, center = T, scale = T){
    x_bar <- rep(0, dim(X)[2])
    X <- scale(X, center, scale)
    
    # Computing the covariance/correlation matrix
    Sigma <- t(X)%*%X
    
    # Getting the eigenvectors and eigenvalues
    eigen <- eigen(Sigma)
    
    return(list(mean = x_bar, D = eigen$values, P = eigen$vector))
    
}

#-------------------------getting the images matrices-------------------
"We are vectorizing the images in order to train the classifier"

library(OpenImageR)
 # dim of the rgb matrix
dim.image <- c(200, 180, 3) # dim of the image


# Getting the names of the images
paths <- list.files('images')

# Since we have the images inside the folder data, we add the data directory to the path in order to make it work
paths <- paste('images',paths, sep = '/')


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
save(images87,file = 'rdata/images87.RData')
save(images95,file = 'rdata/images95.RData')
save(images,file = 'rdata/images.RData')



#---------------------------------BBBBBBBBBBBBB----------------------------------------------

#classifier used to discover the parameters of the classifier
# It takes directly processed images


classifier.beta <- function(parameters, newdata){  # the parameters of knn and pca and the test image
  results <- NULL
  for(f in 1:length(newdata[,1])){
    test <- newdata[f,]
    
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
      if(parameters$threshold < distance){
        uniqv <- 0
        
      }
    }
    
    results <- rbind(results,cbind(predicted.label = uniqv, distance = distance))
    
  }
  return(results)
}


#-----------------------------training classifier beta--------------------------------------
# Loop used to determine the parameters

library(doParallel)
ncores = detectCores()
cl = makeCluster(ncores-1)
registerDoParallel(cl)
pred = NULL
predictions<- NULL 

pred <-foreach(h=seq(1,150,6), '.combine' = rbind )%dopar%{
  library(foreach)
  interval <- h:(h+5)
  data.inv <-  images[interval,] # data is going to be labeled as 0
  data <- images[-interval,]
  k_folds <- 4
  partitions <- sample(rep(1:k_folds, 144/k_folds), replace = F)
  
  
  foreach(i=1:k_folds, '.combine' = rbind)%do%{
    
    train.data <- data[partitions != i,-1]
    train.labels <- data[partitions !=i, 1]
    test.data <- rbind(data[partitions == i,-1],data.inv[,-1])
    test.labels <- c(data[partitions == i, 1], rep(0, length(data.inv[,1])))
    
    parameters = list(x = train.data, y = train.labels, k =1, method = 'euclidean', threshold = 49.89531 , similarity = T, pca.var=87, scale = T, Center = T)
    
    cbind(true.label = test.labels, classifier.beta(parameters, test.data))
  }
  
  
}

stopCluster(cl)
pred<- as.data.frame(pred)
save(pred, file = 'rdata/final_pred.RData')
predictions <- rbind.data.frame(predictions, cbind.data.frame(pred, method = 'manhattan'))
#predictions.pca <- rbind.data.frame(predictions.pca, cbind.data.frame(pred, method ='manhattan 95'))
save(predictions, file = 'rdata/predictions_pca.RData')



#----------------------results b-------------------------------
# PLOTTTING THE RESULTS 

load('rdata/predictions_pca.RData')
predictions <- predictions.pca 

predictions1 <- predictions[predictions$true.label != 0,] # We are just  taking the distances that are rigth
# scaling distances to get the winner
predictions1 <- predictions1[predictions1$true.label == predictions1$predicted.label,]
predictions1$distance[predictions1$method == 'manhattan 87'] <- scale(predictions1$distance[predictions1$method == 'manhattan 87'], center = F, scale = T)
predictions1$distance[predictions1$method == 'manhattan 95'] <- scale(predictions1$distance[predictions1$method == 'manhattan 95'], center = F, scale = T)
predictions1$distance[predictions1$method == 'euclidean 87'] <- scale(predictions1$distance[predictions1$method == 'euclidean 87'], center = F, scale = T)
predictions1$distance[predictions1$method == 'euclidean 95'] <- scale(predictions1$distance[predictions1$method == 'euclidean 95'], center = F, scale = T)

predictions0 <- predictions[predictions$true.label == 0,]
predictions0$distance[predictions0$method == 'manhattan 87'] <- scale(predictions0$distance[predictions0$method == 'manhattan 87'], center = F, scale = T)
predictions0$distance[predictions0$method == 'manhattan 95'] <- scale(predictions0$distance[predictions0$method == 'manhattan 95'], center = F, scale = T)
predictions0$distance[predictions0$method == 'euclidean 87'] <- scale(predictions0$distance[predictions0$method == 'euclidean 87'], center = F, scale = T)
predictions0$distance[predictions0$method == 'euclidean 95'] <- scale(predictions0$distance[predictions0$method == 'euclidean 95'], center = F, scale = T)



ggplot() +
  geom_boxplot(data = predictions1, aes(x = distance, y = method))+
  geom_boxplot(data = predictions0, aes(x =distance, y = method, col = method))

# Winner manhattan 87!!! 

#Using unscaled distances to get the threshold

predictions1 <- predictions[predictions$true.label != 0,]
predictions1 <- predictions1[predictions1$true.label == predictions1$predicted.label,]
predictions0 <- predictions[predictions$true.label == 0,]

predictions1 <- predictions1[predictions1$method == 'manhattan 87',] 
predictions0 <- predictions0[predictions0$method == 'manhattan 87',]

ggplot() +
  geom_density(data = predictions1, aes(x =distance, group = method, col = 'red')) +
  geom_density(data = predictions0, aes(x = distance, col = 'blue')) +
  labs(title = 'Other labels vs 0 label', y = '', x = 'Distance')+
  scale_color_discrete(name = '', labels =c('0', 'Others')) +
  scale_x_continuous(breaks = seq(0, 15000, 500) )+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90))

ggplot() +
  geom_density(data = predictions1, aes(x =distance, group = method, col = 'red')) +
  geom_density(data = predictions0, aes(x = distance, col = 'blue')) +
  labs(title = 'Other labels vs 0 label', y = '', x = 'Distance')+
  scale_color_discrete(name = '', labels =c('0', 'Others')) +
  scale_x_continuous(breaks = seq(0, 12000,1000) )+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = 5362) +
  geom_text(aes(x=5362, label="Threshold", y = 0.0003), colour="black", angle=90, vjust = 1.2)


true.distances  <- predictions1
true.distances <- sort(true.distances[,3])
quantile(true.distances, 0.975)
#6752.141

#---------------------------Finally--------------------------------------------------

parameters.pca = list(x = images87[,-1], 
                  y = images87[,1], 
                  k =1, 
                  method = 'manhattan', 
                  threshold = 6752.141, 
                  similarity = F, 
                  pca.var=87, 
                  scale = T, 
                  Center = T)
save(parameters.pca, file ='rdata/parameters_classifier_pca.RData')

# We ran again the loop of the beginning but using the new parameters and we got
load('rdata/final_pred_pca.RData')
f1 <- pred.pca[pred.pca[,1] == pred.pca[,2],]
f0 <- pred.pca[pred.pca[,1] != pred.pca[,2],]
ggplot() +
  geom_boxplot(data =f1, aes(y = distance, x = true.label, group = true.label), col = 'blue') +
  geom_boxplot(data = f0, aes(x = true.label, y = distance, group=true.label,col = predicted.label))+
  geom_hline(yintercept = 6752.141) +
  scale_x_continuous(breaks = seq(0, 25,1))

#With An acuraccy of 0.867619 in training data, we hope it performs better on
sum(pred.pca[,1] == pred.pca[,2])/length(pred.pca[,1])



#-----------------------Building the classifier----------------------------
# Same as the classifier beta but taking into account pca and the vectorization of an image

classifier <- function(parameters, newImage){#Takes only one path
  
  rgb.vector <- readImage(newImage)
  rgb.vector <- as.numeric(as.vector(rgb.vector))
  
  # PCA
  if(parameters$pca.var == 100){
    test <- rgb.vector 
  }else{
  
    dim(rgb.vector) <- dim.rgb # Building the rgb matrix
    comps <- pca(rgb.vector, center = parameters$center, scale = parameters$scale) # Applying pca
    if (parameters$pca.var == 95){
      # Saving the image with  95 percent of variance
      pcaIm95<- as.vector(rgb.vector%*%comps$P[,1:2])
    } else if(parameters$pca.var == 87){
      test <- as.vector(rgb.vector%*%comps$P[,1])
    }else{
      print('Percentage of variance not supported')
      return(NULL)
    }
  }
  
  # KNN
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
    if(parameters$threshold < distance){
      uniqv <- 0
      
    }
  }
  
  if(parameters$similarity == T){
    return(cbind(uniqv, distance))
  }
  return(uniqv)
}


save(classifier, pca, dim.rgb,file = 'rdata/classifier.RData')






#---------------------------------DDDDDDDD-------------------------------------------
# Used beta classifier and got same k = 1, and:

#------------------------------------results ---------------------------------
predictions <- load('rdata/predictions')

predictions1 <- predictions[predictions$true.label != 0,] # We are just  taking the distances that are rigth
# scaling distances to get the winner
predictions1 <- predictions1[predictions1$true.label == predictions1$predicted.label,]
predictions1$distance[predictions1$method == 'manhattan'] <- scale(predictions1$distance[predictions1$method == 'manhattan'], center = F, scale = T)
predictions1$distance[predictions1$method == 'euclidean'] <- scale(predictions1$distance[predictions1$method == 'euclidean'], center = F, scale = T)

predictions0 <- predictions[predictions$true.label == 0,]
predictions0$distance[predictions0$method == 'manhattan'] <- scale(predictions0$distance[predictions0$method == 'manhattan'], center = F, scale = T)
predictions0$distance[predictions0$method == 'euclidean'] <- scale(predictions0$distance[predictions0$method == 'euclidean'], center = F, scale = T)


ggplot() +
  geom_boxplot(data = predictions0, aes(x = distance, y = method))+
  geom_boxplot(data = predictions1, aes(x =distance, y = method, col = method))

# Winner euclidean

#Using unscaled distances to get the threshold

predictions1 <- predictions[predictions$true.label != 0,]
predictions1 <- predictions1[predictions1$true.label == predictions1$predicted.label,]
predictions0 <- predictions[predictions$true.label == 0,]

predictions1 <- predictions1[predictions1$method == 'euclidean',] 
predictions0 <- predictions0[predictions0$method == 'euclidean',]

ggplot() +
  geom_density(data = predictions1, aes(x =distance, group = method, col = 'red')) +
  geom_density(data = predictions0, aes(x = distance, col = 'blue')) +
  labs(title = 'Other labels vs 0 label', y = '', x = 'Distance')+
  scale_color_discrete(name = '', labels =c('0', 'Others')) +
  scale_x_continuous(breaks = seq(0, 15000, 500) )+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90))

true.distances  <- predictions1
true.distances <- sort(true.distances[,3])
quantile(true.distances, 0.975)
49.895

#---------------------------Finally--------------------------------------------------

parameters = list(x = images[,-1], 
                      y = images[,1], 
                      k =1, 
                      method = 'manhattan', 
                      threshold = 50, 
                      similarity = F, 
                      pca.var=100, 
                      scale = F, 
                      Center = F)
save(parameters, file ='rdata/parameters_classifier.RData')

# We ran again the loop of the beginning but using the new parameters and we got
load('rdata/final_pred.RData')
f1 <- pred[pred[,1] == pred[,2],]
f0 <- pred[(pred[,1] != pred[,2]),]
ggplot() +
  geom_boxplot(data =f0, aes(y = distance, x = true.label, group = true.label), col = 'blue') +
  geom_boxplot(data = f1, aes(x = true.label, y = distance, group = true.label))+
  geom_hline(yintercept = 49.895) +
  scale_x_continuous(breaks = 0:25)

#An acuraccy of
sum(pred[,1] == pred[,2])/length(pred[,1])
#0.9538

