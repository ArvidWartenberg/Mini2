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
  
  library(caret) # confusionMatrix, createFolds
}

{# <- brackets around whole program
evaluationF<-function(predicted,actual){
  
  actualOL<-  actual # Opposite labels
  if(length(levels(predicted))>=2){
    
    majorityPred <- sort(table(predicted),decreasing=TRUE)
    majorityPred1st <- rownames(majorityPred)[1]
    majorityPred2nd <- rownames(majorityPred)[2]
    
    # We assume that the majority predicted labels are the majority labels in actual
    if(nC1>=nC2){
      levels(actual)<-c(majorityPred1st,majorityPred2nd)   # assuming class 1 is labeled 1
      levels(actualOL)<-c(majorityPred2nd,majorityPred1st)
    }
    levels(actual)<-union(levels(actual),levels(predicted))
    levels(actualOL)<-union(levels(actualOL),levels(predicted))
    actual <- factor(actual,levels = levels(predicted))# Reordering of labels to be the same as for the predicted
    actualOL <- factor(actualOL,levels = levels(predicted))# Reordering of labels to be the same as for the predicted
    
    cm <- confusionMatrix(table(predicted,actual))$byClass
    cmOL <- confusionMatrix(table(predicted,actualOL))$byClass
    
    if(length(levels(predicted))>2){
      cm <- (cm[paste("Class:",majorityPred1st),] + cm[paste("Class:",majorityPred2nd),])/2
      balancedAcc <- cm["Sensitivity"]
      
      cmOL <- (cmOL[paste("Class:",majorityPred1st),] + cmOL[paste("Class:",majorityPred2nd),])/2
      balancedAccOL <- cmOL["Sensitivity"]
      
      balancedAcc <- max(balancedAcc,balancedAccOL)
      
    }else{ # Only two predicted classes
      cm <- confusionMatrix(table(predicted,actual))$byClass
      cmOL <- confusionMatrix(table(predicted,actualOL))$byClass
      
      balancedAcc <- cm["Balanced Accuracy"]
      balancedAccOL <- cmOL["Balanced Accuracy"]
      
      balancedAcc <- max(balancedAcc,balancedAccOL)
    }
    
  }else{ # Only one class
    if(nC1>=nC2){
      levels(predicted)<-"1"   # assuming actual class 1 is labeled 1
    }else{
      levels(predicted)<-"2"
    }
    levels(predicted)<-union(levels(actual),levels(predicted))
    predicted <- factor(predicted,levels = levels(actual))# Reordering of labels to be the same as for the actual
    
    cm <- confusionMatrix(table(predicted,actual))$byClass
    balancedAcc <- cm["Balanced Accuracy"]
  }
  
  balancedAcc
}
{
  maxDist <- 30
  maxItr <- 50
  dDist <- maxDist/maxItr
  nFolds <- 10
  
  errorMat<- as.data.frame(matrix(0,maxItr,6))
  colnames(errorMat) <- c("K-means","K-means 30","Bottom Up Average","Bottom Up Ward's","GMM","Density")
}


for(i in 1:maxItr){
  dist <- dDist*i;
  # ------ Simulate data same dim space ------
  nDim <- 3; nC1 <- 180; nC2 <- 20;
  muC1 <- rep(0,nDim); sigmaC1 <- 10; sigmaMatC1 <- sigmaC1*diag(nDim);
  muC2 <- rep(0,nDim); muC2[1] <- dist; sigmaMatC2 <- sigmaMatC1;
  #set.seed(10)
  xC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
  xC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)

  xC1 <- cbind(rep(1,nC1),xC1)
  xC2 <- cbind(rep(2,nC2),xC2)

  X<-rbind(xC1,xC2)

  # Standardize!
  X[,-1] <- scale(X[,-1])
  X <- as.data.frame(X); X[,1]<-factor(X[,1])


  # ------ Simulate data different dim space ------
  # nDim <- 3; nC1 <- 180; nC2 <- 20;
  # muC1 <- rep(0,2*nDim); sigmaC1 <- 10; sigmaMatC1 <- sigmaC1*diag(2*nDim); sigmaMatC1[1:nDim,1:nDim]<-matrix(0,nDim);
  # muC2 <- rep(0,2*nDim); sigmaC2 <- 10; muC2[1] <- dist; sigmaMatC2 <- sigmaC2*diag(2*nDim);sigmaMatC1[1,1]<-0;sigmaMatC1[2,2]<-0;
  # sigmaMatC1[(nDim+1):(2*nDim),(nDim+1):(2*nDim)]<-matrix(0,nDim);
  # #set.seed(10)
  # xC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
  # xC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
  # 
  # xC1 <- cbind(rep(1,nC1),xC1)
  # xC2 <- cbind(rep(2,nC2),xC2)
  # 
  # X<-rbind(xC1,xC2)
  # 
  # # Standardize!
  # X[,-1] <- scale(X[,-1])
  # X <- as.data.frame(X); X[,1]<-factor(X[,1])
  # 

  
  # ------ Crossvalidation -------
  foldIndices <-createFolds(X[,1],nFolds)
  
  for(fold in 1:nFolds){
    
    # ------ K-means ------
    km <- kmeans(x=X[-foldIndices[[fold]],-1] , centers=2, iter.max = 30, nstart = 1,
                 algorithm = c("Lloyd"), trace=FALSE)

    predicted <- factor(km$cluster)
    actual <- X[-foldIndices[[fold]],1]
    # ------ Evaluation ------
    errorMat[i,1] <- errorMat[i,1] +evaluationF(predicted,actual)


    # ------ K-means 30 start------
    km <- kmeans(x=X[-foldIndices[[fold]],-1] , centers=2, iter.max = 30, nstart = 30,
                 algorithm = c("Lloyd"), trace=FALSE)

    predicted <- factor(km$cluster)
    actual <- X[-foldIndices[[fold]],1]
    # ------ Evaluation ------
    errorMat[i,2] <- errorMat[i,2] +evaluationF(predicted,actual)


    # # ------ Bottom Up Average ------
    D <- dist(X[-foldIndices[[fold]],-1],method="euclidean")
    hc <- hclust(D, method = "average", members = NULL)  # Method = single/average/complete


    actual <- X[-foldIndices[[fold]],1]
    # ------ Evaluation ------
    # Best in 10
    maxAcc <- 0
    for(j in 2:10){
      predicted <- factor(cutree(hc,k=j))

      tmpAcc<-evaluationF(predicted,actual)
      if( maxAcc<tmpAcc ){
        maxAcc<-tmpAcc
      }
    }

    errorMat[i,3] <- errorMat[i,3] + maxAcc


    # ------ Bottom Up Ward's method ------
    D <- dist(X[-foldIndices[[fold]],-1],method="euclidean")
    hc <- hclust(D, method = "ward.D", members = NULL)  # Method = single/average/complete

    actual <- X[-foldIndices[[fold]],1]
    # ------ Evaluation ------
    # Best in 10
    maxAcc <- 0
    for(j in 2:10){
      predicted <- factor(cutree(hc,k=j))

      tmpAcc<-evaluationF(predicted,actual)
      if( maxAcc<tmpAcc ){
        maxAcc<-tmpAcc
      }
    }

    errorMat[i,4] <- errorMat[i,4] + maxAcc


    # ------ Gaussian Mixture Models ------

    gmm_clust <- Mclust(X[-foldIndices[[fold]],-1], 2);

    predicted <- factor(gmm_clust$classification)
    actual <- X[-foldIndices[[fold]],1]
    # ------ Evaluation ------
    errorMat[i,5] <- errorMat[i,5] +evaluationF(predicted,actual)


    # ------ Density Based Clustering ------
    
    db <- dbscan::dbscan(X[-foldIndices[[fold]],-1], eps = 0.8, minPts = 10)
    
    
    predicted <- factor(db$cluster)
    actual <- X[-foldIndices[[fold]],1]
    # ------ Evaluation ------
    errorMat[i,6] <- errorMat[i,6] + evaluationF(predicted,actual)
    
  }
  errorMat[i,] <- errorMat[i,]/nFolds   # Average over crossvalidation folds
  
}

  # ----- Plot Errors -----
  {
    p<-ggplot()
    p <- p + geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,1], color = colnames(errorMat)[1])) +
      geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,2], color = colnames(errorMat)[2])) +
      geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,3], color = colnames(errorMat)[3])) +
      geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,4], color = colnames(errorMat)[4])) +
      geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,5], color = colnames(errorMat)[5])) +
      geom_point(data=errorMat, aes(x = (1:maxItr)*dDist, y = errorMat[,6], color = colnames(errorMat)[6])) +
      xlab("distance")+ ylab("balanced accuracy") + labs(color='method')
    plot(p)
  }

}# <- brackets around whole program


