#' Function to convert all relevant columns to numeric
#' Create a plain data frame without GUID and category
#' 
#' @param x data frame to pass through
#' @keywords convert to numeric
#' @export
#' @examples 


#Function to convert all relevant columns to numeric
#Create a plain data frame without ID and category
plain_num <- function(x){
  x[2:ncol(x)]<-lapply(x[2:ncol(x)],as.numeric)
  x[is.na(x)]<-99
  plainnum <- dplyr::select(x,-c(GUID,category))
  plainnum
}


#' Function to create a loadings file from the factanal() output
#' 
#' @param x factanal() model
#' @keywords factor analysis
#' @export
#' @examples 

#At factor level
fa.load<-function(x,file_name=NULL,writeCSV=FALSE){
  x <- as.matrix(x$loadings)
  loadings.max <- colnames(x)[apply(x,1,which.max)] #Find max and return column header
  x<-cbind(x,loadings.max)
  if(writeCSV==TRUE){write.csv(x,file_name)}
  x
}

#' Function to create a score file from the factanal() output
#' 
#' @param x factanal() model
#' @keywords factor analysis
#' @export
#' @examples 

fa.score<-function(x,file_name=NULL,writeCSV=FALSE){
  x <- as.matrix(x$scores)
  scores.max <- colnames(x)[apply(x,1,which.max)] #Find max and return column header
  x<-cbind(x,scores.max)
  if(writeCSV==TRUE){write.csv(x,file_name)}
  x
}