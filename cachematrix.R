## Homework for Coursera course R Programming
## by Steven Zwart
##
## Create functions to store a the inverse of a matrix
## in cache, so that it will not have to be recalculated
## if called a second time

## This function creates a special matrix
## that stores the inverse in cache, so that
## the inverse doesn't have to be calculated each time
## the function is called.

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(B) {
    x <<- B
    M <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) M <<- solve
  getInverse <- function() M
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function calculates the inverse of a matrix created by makeCacheMatrix
## If the inverse has already been calculated it will retrieve the inverse 
## matrix from the cache.

cacheSolve <- function(x, ...) {
  M <- x$getInverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setInverse(M)
  M
}
