## makeCacheMatrix returns an object that can cache the inverse of a specified matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve asks if the inverse is null, meaning it hasn't been cached yet, and computes it if so.
## Otherwise, it returns the cached inverse.  This function passes any additional arguments on to solve.

cacheSolve <- function(x, ...) {
  if (is.null(x$getInverse())) {
    i <- solve(x$get(), ...)
    x$setInverse(i)
    return(i)
  }
  x$getInverse()
}
