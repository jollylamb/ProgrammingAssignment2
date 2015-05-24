## The purpose of this code is to calculate the inverse 
## of a matrix. If the inverse of a matrix has already 
## been calculated, it will be returned instead of 
## recalculated.

## This function makes the matrix and get/set type functions.
## It returns a matrix that can be passed to the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  
  set <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(myMatrix) inverseM <<- myMatrix
  getInverse <- function() inverseM
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## if the inverse of the matrix x has already been 
## computed, that inverse will be returned. 
## Otherwise, the inverse will be computer and returned.
cacheSolve <- function(x, ...) {
  im <- x$getInverse()
  
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  message("creating cached data")
  
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
}
