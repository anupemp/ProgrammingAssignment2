## makeCacheMatrix creates a special matrix object in cache.
## To calculate Matrix inversion, it is usually a costly computation. So instead of repeated calculation, it will 
## find it in the cache and return it, and not calculate it again.


makeCacheMatrix <- function(x = matrix()) { 
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
