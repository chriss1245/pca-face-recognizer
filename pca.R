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
























# Profe
data=matrix(rnorm(12*2),12,2)

labels=rep(1:4,each=3)

test=rnorm(2)



dmatrix=dist(rbind(test,data), method = "euclidean", diag = TRUE, upper = TRUE)

dmatrix=as.matrix(dmatrix)

dmatrix=dmatrix[1,2:(nrow(data)+1)]

ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)

labels_sel=labels[ordenados$ix[1:3]]

uniqv <- unique(labels_sel)

votes <- tabulate(match(labels_sel, uniqv))

uniqv <- uniqv[which(votes == max(votes))] 
max(votes)

if(length(uniqv) > 1) uniqv <- 0


