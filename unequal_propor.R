# generate a dataset with 4 different classes
# generate dataset with unequal sizes
library(RColorBrewer)

brewer.pal(n = 4, name = "PRGn")
palette(brewer.pal(n = 4, name = "PRGn"))



set.seed(20000)
data_size = 10000
x = matrix(rnorm(data_size * 2), data_size, 2)
xmean = matrix(rnorm(2, sd = 40), 2, 2)
which_unequal = sample(c(1,2), data_size, replace = TRUE, prob = c(0.04, 0.96))
which = sample(c(1,2), data_size, replace = TRUE)
x_unequal = x + xmean[which_unequal, ]
x = x + xmean[which, ]
par(mfrow=c(1,2)) 
plot(x, col = which, pch = 19)
title("equal proprotions")
plot(x_unequal, col = which_unequal, pch = 19)
title("unqual proportions 4% 96%")


## apply k-means 
km_unequal.out = kmeans(x_unequal, 2, nstart = 15)
km.out = kmeans(x, 2, nstart = 15)
km_unequal.out
km.out



## plot the result
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))

plot(x[c(which == km.out$cluster)], col = km.out$cluster, cex = 2, pch = 1, lwd = 2, ylab = "", xlab = "")
points(x[c(which == km.out$cluster)], col = c(1,2)[which], pch = 19)
title ("equal case")

plot(x_unequal[c(which_unequal == km_unequal.out$cluster)], col = km_unequal.out$cluster, cex = 2, pch = 1, lwd = 2, xlab = "", ylab = "")
points(x_unequal[c(which_unequal == km_unequal.out$cluster)], col = which_unequal, pch = 19)
points(x_unequal, col = c(2,1)[which_unequal], pch = 19)
title ("unequal case")
mtext("Missclassifications", outer = TRUE, cex = 1.5)



par(mfrow=c(1,2)) 
plot(x, col = km.out$cluster, cex = 2, pch = 1, lwd = 2, ylab = "")
points(x, col = c(2,1)[which], pch = 19)

plot(x_unequal, col = km_unequal.out$cluster, cex = 2, pch = 1, lwd = 2)
points(x_unequal, col = which_unequal, pch = 19)
points(x_unequal, col = c(2,1)[which_unequal], pch = 19)




## tabling 
table(which_unequal, km_unequal.out$cluster)

table(which, km.out$cluster)
