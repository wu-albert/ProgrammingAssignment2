## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a list of functions
## to perform the following
## 1. set: sets the matrix, and resets cached data
## 2. get: retrieves the matrix
## 3. setInverse: calculates the inverse, stores into the cache
## 4. getInverse: retrieves the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
