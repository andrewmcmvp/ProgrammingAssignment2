## makeCacheMatrix creates a special "vector", 
## which is really a list containing the following four functions
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the inverse matrix
## 4) get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Calculates the inverse matrix of the special "vector" created 
## with the makeCacheMatrix function. It first checks to see if the
## mean has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates 
## the inverse matrix of the data and sets the value of the 
## inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {

      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
