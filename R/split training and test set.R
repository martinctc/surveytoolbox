#'Split the data into a simple training and testing set
#'
#'@param x Pass your data frame or matrix here.
#'@param part A numeric value between 0 and 1 to represent the proportion of the whole data you want to use as the training set.
#'@keywords train, test
#'@examples
#'x <-as.data.frame(matrix(1:5000,250,20))
#'x.train <- split_tt(x,.7)$train
#'x.test <- split_tt(x,.7)$test
#'
#'dim(x.train)
#'dim(x.test)


split_tt <-function(x, part){
  rowz <- nrow(x)
  samp <- sample(seq_len(rowz),floor(rowz * part))
  output <-list("train"=x[samp,],"test"=x[-samp,])
  output
}

#'x <-as.data.frame(matrix(1:5000,250,20))
#'x.train <- split.tt(x,.7)$train
#'x.test <- split.tt(x,.7)$test
#'
#'dim(x.train)
#'dim(x.test)
