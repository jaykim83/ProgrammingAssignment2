## makeCacheMatrix and cacheSolve creates and computes the inverse of a matrix and stores ## it in the cache for faster processing when the same value is needed in the future.



## Returns a vector containing four functions that set the matrix, get the matrix and set and get the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)      
      
}


## Computes the inverse of the matrix that was created through the set() function in makeCacheMatrix and stores it in the cache.

cacheSolve <- function(x, ...) {
      
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
      
}
