## Contains functions to calculate, and store an inverse of a provided matrix.
## Since matrix caclulations can be costly computationally, these functions
## create a list data structure to cache the results of the inverse function.

## makeCacheMatrix():  Create data structure to store and retrieve cached inverse
##                     of matrix calculations.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve():  Calculates inverse of a matrix and stores in data structure
##                created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata, ...)
  x$setinverse(inv)
  inv
}
