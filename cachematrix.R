# This file contains functions for creating and working with a cached matrix, 
# that is, a matrix capable of having its inverse associated with it.

# This function creates a matrix capable of caching its inverse. It optionally accepts
# an existing matrix as an input argument.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # getter/setter functions for the matrix itself
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # getter/setter functions for caching the inverse
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


# This function returns the inverse of a matrix x. If x has its inverse cached, the
# cached value is returned. If not, the inverse is computed and then cached.
cacheSolve <- function(x, ...) {
    # Pull the inverse from the cache
    inv <- x$getinverse()
    
    # If the cache contained the inverse, we're done
    if (!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    
    # Calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache it
    x$setinverse(inv)
    
    inv
}
