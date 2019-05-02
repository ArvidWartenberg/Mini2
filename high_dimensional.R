library(dplyr)
library(Matrix)
library(MASS)
library(Rtsne)
library(irlba)
library(R.utils)
library(dplyr)
library(purrr)
library(klaR)
library(ggplot2)
library(latex2exp)
library(viridisLite)
library(viridis)
library(dbscan)
library(wesanderson)




download.file("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",
              "train-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",
              "train-labels-idx1-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
              "t10k-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
              "t10k-labels-idx1-ubyte.gz")

# gunzip the files
R.utils::gunzip("train-images-idx3-ubyte.gz")
R.utils::gunzip("train-labels-idx1-ubyte.gz")
R.utils::gunzip("t10k-images-idx3-ubyte.gz")
R.utils::gunzip("t10k-labels-idx1-ubyte.gz")

# helper function for visualization
show_digit = function(arr784, col = gray(12:1 / 12), ...) {
  image(matrix(as.matrix(arr784[-785]), nrow = 28)[, 28:1], col = col, ...)
}

# load image files
load_image_file = function(filename) {
  ret = list()
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}

# load images
train = load_image_file("train-images-idx3-ubyte")
test  = load_image_file("t10k-images-idx3-ubyte")

# load labels
train$y = as.factor(load_label_file("train-labels-idx1-ubyte"))
test$y  = as.factor(load_label_file("t10k-labels-idx1-ubyte"))

# view test image
show_digit(train[10000, ])

# Choosing training data
train_data <- as_tibble(train) %>%
  filter(y %in% c(0, 8, 9)) %>%
  mutate(y = as.factor(y)) %>%
  droplevels()



test_data <- as_tibble(test) %>%
  filter(y %in% c(0, 8, 9)) %>%
  mutate(y = as.factor(y)) %>%
  droplevels()


colors = magma(length(unique(train_data$y)))
names(colors) = unique(train_data$y)


tsne <- Rtsne(train_data[,], dims = 2, check_duplicates = FALSE, verbose=TRUE, max_iter = 500, partial_pca=TRUE)


plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=train_data$y, col=colors[train_data$y])


cl <- dbscan(tsne$Y, eps = 0.5 ,minPts = 5)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


ggplot(train_data$y, aes(x = tsne$Y[,1], y = tsne$Y[,2], color = cl$cluster)) + 
  geom_point(size = 2) +  
  scale_colour_manual(values=cbp1)


