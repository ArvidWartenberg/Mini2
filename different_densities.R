library(ggplot2)

brewer.pal(n = 4, name = "PRGn")
palette(brewer.pal(n = 4, name = "PRGn"))



set.seed(20000)
rm(list = ls())
set.seed(1234)

n <- 1000
mu1 <- c(0,0)# Mean
mu2 <- c(6,6)
Sigma1 <- matrix(c(1, .5, .5, 1), 2)  # Covariance matrix
Sigma2 <- matrix(c(5, 0.1, 0.1, 5), 2) 

x1 <- mvrnorm(n, mu = mu1, Sigma = Sigma1 )
data1 <- data.frame(x = x1[,1], y = x1[,2], group="A")

x2 <-mvrnorm(n, mu = mu2, Sigma = Sigma2 )
data2 <-  data.frame(x = x2[,1], y = x2[,2], group="B")



data <- rbind(data1,data2)

ggplot(data, aes(x=x, y=y,colour=group)) + geom_point(size=1.5, alpha=.6) 


x_clustered_db <- dbscan(data.matrix(data), eps = 0.2, minPts = 10)

ggplot(data,aes(x=x, y = y,colour = x_clustered_db$cluster))  + geom_point(size=1.5, alpha=1) 


x_clustered_kmeans <- kmeans(data.matrix(data),2)
ggplot(data,aes(x=x, y = y,colour = x_clustered_kmeans$cluster))  + geom_point(size=1.5, alpha=1) 

