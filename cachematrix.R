## Programming assignment 2 (Coursera)
## Storing matrix inverse in separate environment to save computing time

## 1st Function: creates matrix that can store its inverse in a separate environment

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
setMatrix <- function(y) {
  x <<- y
  inv <<- NULL
}
setMatrix(x)
getMatrix <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(setMatrix, getMatrix, setInverse, getInverse)
}


## computes inverse from above function
## if inverse already computed, retrieve it from the cache

cacheSolve <- function(x, ...) {
  ## check if matrix is already computed
  isSolved <- x$getInverse()
  if (!is.null(isSolved)) {
    message("getting cached data")
    return(isSolved)
  }
  
  #matrix is not already computed, calculate inverse and cache
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
