## Matrix inversion is usually a costly computationand there may be some benefit to caching the inverse
##of a matrix rather than compute it repeatedly. The assignment is 
##to write a pair of functions that cache the inverse of matrix.

## This function creates a specal "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has
##already been calculates (and the matrix has not changed), then
##the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
