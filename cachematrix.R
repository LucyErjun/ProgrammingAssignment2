## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(...)) {
rm <- NULL
   cm <-NULL
   set <- function(y) {
   x <<- y
   rm <<- NULL
   cm <<-NULL
   }
  get <- function() x
  setRowMean <- function(rmean) rm <<- rmean
  getRowMean <- function() rm
  setColMean <- function(cmean) cm <<- cmean
  getColMean <- function() cm
  list(set = set, get = get,
           setRowMean = setRowMean,
           getRowMean = getRowMean,
           setColMean = setColMean,
           getColMean = getColMean
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        rm<-x$getRowMean()
  cm<-x$getColMean()
  if(!is.null(rm)&!is.null(cm)) {
    message("getting cached data of mean")
    return(list(rowMean=rm,colMean=cm)) 
  } 
  data<-x$get()
  rm<-rowMeans(data,...)
  cm<-colMeans(data,...)
  x$setRowMean(rm)
  x$setColMean(cm)
  list(rowMean=rm,colMean=cm)
}
