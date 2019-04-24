{ #<---------- Collapse Libararies Here
  closeAllConnections()
  rm(list=ls())              # Clear variables
  graphics.off()             # Clear plot windows
  
  # General purpose packages (data handling, pretty plotting, ...)
  library(tidyverse)
  library(latex2exp) # Latex in ggplot2 labels
  library(kableExtra) # Pretty printing for tables
  cbPalette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour-blind friendly palette
  # Packages for actual computation
  library(MASS)            # Contains mvrnorm to simulate multivariate normal
  #                          # Use as MASS::mvrnorm is recommended since
  #                          # MASS::select clashes with dplyr::select
  
  library(DAAG)              # Contains the `monica` dataset
  library(ElemStatLearn)     # Contains the `SAheart` dataset
  library(robustHD)
  set.seed(282410)        # set a seed to make results reproducible
  
  library(dendextend)  # Many more possibilities for the customization of
  # dendrograms
  library(RCurl)
  library(dplyr)
  
  library(rpart)
  library(gplots)           # Heatmap 2
  library(ggplot2)
  library(papeR)
  
  library(mclust)     # Gaussian Mixture Model
  
  # Density Based Clustering 
  library(fpc)
  library(dbscan)
}

{
percentError <- function(cluster,nC1,nC2){
  
  majorityC1 <- majorityVote(cluster[1:nC1])
  majorityVoteInC1 <- majorityC1$table[majorityC1$majority]
  
  if(majorityVoteInC1 = nC1){
    
    majority <- majorityVote(cluster)
    
    C1Votes <- 
    C2Votes <- majorityVoteInC1-nC1
  }
  # percentErrorC1 <- 1-majorityVotesC1/nC1
  # 
  # majorityC2 <- percentErrorC1
  # majorityVotesC2 <- majorityC1$table[majorityC2$majority]
  # percentErrorC2 <- 1-majorityVotesC2/nC2
  # error <- percentErrorC1/nC1 + percentErrorC2/nC2 
  
  5
}

maxDist <- 10000
dDist <- 100
maxItr <- maxDist/dDist

errorMat <- as.data.frame(matrix(0,maxItr,4))
colnames(errorMat) <- c("k-means","bottom up","GMM","density")
}
for(i in 1:maxItr){
  dist <- dDist*i;
# ------ Simulate data 1 ------
  nDim <- 2; nC1 <- 100; nC2 <- 100;
  muC1 <- rep(0,nDim); sigmaC1 <- 10; sigmaMatC1 <- sigmaC1*diag(nDim);
  muC2 <- rep(0,nDim); muC2[1] <- dist; sigmaMatC2 <- sigmaMatC1;
  
  xC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
  xC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
  
  xC1 <- cbind(rep(1,nC1),xC1)
  xC2 <- cbind(rep(2,nC2),xC2)
  
  X<-rbind(xC1,xC2)
  
  # Standardize!
  X[,-1] <- scale(X[,-1])
  X <- as.data.frame(X); X[,1]<-factor(X[,1])
  
  # ---- Simple Data Plot ----
  # p <- ggplot(X[1:nC1,], aes(x = X[1:nC1,2], y = X[1:nC1,3])) +
  #   geom_point(aes(color = '1')) + xlab("x") + ylab("y")
  # p <- p + geom_point(data=X[(nC1+1):(nC1+nC2),], mapping=aes(x = X[(nC1+1):(nC1+nC2),2], y = X[(nC1+1):(nC1+nC2),3],color = '2')) +
  #   xlab("x") + ylab("y") + labs(color='Cluster')  + ggtitle("Data")
  # 
  # plot(p)


  
# ------ Simulate data 2 ------
# nDim <- 2; dist <- 100; n <- 100;
# muC1 <- rep(0,nDim); sigmaC1 <- 10; sigmaMatC1 <- sigmaC1*diag(nDim);
# muC2 <- rep(0,nDim); muC2[1] <- dist; sigmaMatC2 <- sigmaMatC1;
# 
# xC1<-MASS::mvrnorm(n = n, muC1, sigmaMatC1, empirical = FALSE)
# xC2<-MASS::mvrnorm(n = n, muC2, sigmaMatC2, empirical = FALSE)
# 
# 
# xC1 <- cbind(rep(1,n),xC1,matrix(0,n,nDim+1))
# xC2 <- cbind(rep(2,n),matrix(0,n,nDim+1),xC2)
# 
# X<-rbind(xC1,xC2)
#   
# # Standardize!
# X[,-1] <- scale(X[,-1])
# X <- as.data.frame(X); X[,1]<-factor(X[,1])
# 
# p <- ggplot(X[1:n,], aes(x = X[1:n,2], y = X[1:n,3])) +
#   geom_point(aes(color = '1')) + xlab("x") + ylab("y")
# 
# p <- p + geom_point(data=X[(n+1):(2*n),], mapping=aes(x = X[(n+1):(2*n),2], y = X[(n+1):(2*n),3],color = '2')) +
#   xlab("x") + ylab("y") + labs(color='Cluster')  + ggtitle("Data")
#   
# plot(p)
  

# ------ K-means ------

km <- kmeans(x=X[,-1] , centers=2, iter.max = 30, nstart = 1,
             algorithm = c("Lloyd"), trace=FALSE)

errorMat[i,1] <- percentError(km$cluster,nC1,nC2)
#errorMat[i,1] <- 10
  
# ---- test plot ----
# clusters <- cbind(km$cluster,X[,-1])
# p2 <- ggplot(data=clusters, aes(x = clusters[,2], y = clusters[,3], color = as.factor(clusters[,1]))) + geom_point() +
#       xlab("x") + ylab("y") + labs(color='Cluster')  + ggtitle("Data")
# plot(p2)




# ------ Bottom Up ------

D <- dist(X[,-1],method="euclidean")
hc <- hclust(D, method = "single", members = NULL)  # Method = single/average/complete

#plot(hc)
errorMat[i,2] <- percentError(cutree(hc,2),nC1,nC2)
#errorMat[i,2] <- 20

# ---- test plot ----
# clusters <- cbind(cutree(hc,2),X[,-1])
# p2 <- ggplot(data=clusters, aes(x = clusters[,2], y = clusters[,3], color = as.factor(clusters[,1]))) +
#   xlab("x") + ylab("y")+geom_point() + labs(color='Cluster') 
# 
# plot(p2)



 # ------ Gaussian Mixture Models ------

gmm_clust <- Mclust(X[,-1], 2)

errorMat[i,3] <- percentError(gmm_clust$classification,nC1,nC2)
#errorMat[i,3] <- 30  


# ----- test plot -----
# clusters <- cbind(gmm_clust$classification,X[,-1])
# p2 <- ggplot(data=clusters, aes(x = clusters[,2], y = clusters[,3], color = as.factor(clusters[,1]))) +
#   xlab("x") + ylab("y")+geom_point() + labs(color='Cluster') + ggtitle("Gaussian Mixture Model")
# plot(p2)


# ------ Density Based Clustering ------

db <- dbscan::dbscan(X[,-1], eps = 0.5, minPts = 5)

errorMat[i,4] <- percentError(db$cluster,nC1,nC2)
#errorMat[i,4] <- 40  


# ---- test plot -----
# clusters <- cbind(db$cluster,X[,-1])
# p2 <- ggplot(data=clusters, aes(x = clusters[,2], y = clusters[,3], color = as.factor(clusters[,1]))) +
#   xlab("x") + ylab("y")+geom_point() + labs(color='Cluster') + ggtitle("Density Based")
# plot(p2)

}


# ----- Plot Errors -----
{
p<-ggplot()
p <- p + geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,1], color = colnames(errorMat[1]))) +
  geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,2], color = colnames(errorMat[2]))) +
  geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,3], color = colnames(errorMat[3]))) +
  geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,4], color = colnames(errorMat[4]))) +
  xlab("distance")+ ylab("Error") + labs(color='method')
plot(p)
}

sum(errorMat[,1])
sum(errorMat[,2])
sum(errorMat[,3])
sum(errorMat[,4])

errorMat[,1]


# ---- test plot ----
# p<-ggplot()
# testMat <- as.data.frame(matrix(0,10,4))
# for(i in 1:10){
#   for(j in 1:4){
#     testMat[i,j]<-i*j
#   }
# }
# 
# p<-ggplot()
# 
# methodName <- colnames(testMat)[1]
# p <- p + geom_point(data=testMat, aes(x = 1:10, y = testMat[,1],color = methodName))
# p <- p + geom_point(data=testMat, aes(x = 1:10, y = testMat[,2]))
# p<-p+xlab("distance")+ ylab("Error") + labs(color='Method')
# plot(p)
# 
# 
# 

