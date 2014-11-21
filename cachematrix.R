## Combined, the pair of functions below cache the inverse of a matrix.
  ## makeCacheMatrix creates the matrix and caches the inverse while
  ## cacheSolve computes the inverse of the matrix if the inverse
  ## has not already been cache'd by makeCacheMatrix.

## This function creates a special "matrix" object that
## can cache its inverse using the solve function.

makeCacheMatrix <- function(x = matrix()){
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

## The cacheSolve function computes the inverse of the special "matrix"
  ## returned by makeCacheMatrix above.
## If the inverse has already been calculated
  ## (and the matrix has not changed), then cacheSolve should
  ## retrieve the inverse from the cache.

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
