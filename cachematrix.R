## Below are two functions that can be used to cache the inverse of matrix
## makeCacheMatrix is used to create special  "matrix" type
## cacheSolve finds or upload from cache the inverse of a the result of makeCacheMatrix


## This function creates a "matrix" object, which can be used to cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  ## the result of function is a list containing containing a function to
  ## set the matrix
  ## get the matrux
  ## set the inverse matrix
  ## get the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  This function takes "matrix" object x (result of makeCacheMatrix) and returns its inverse. 
##  For the first time inverse is calculated, while for next calls it is retrieved from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## if inverse was already found, it is retrieved from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if inverse was not calculated yet, it is calculated, saved and returned
  matrixToBeInversed <- x$get()
  m <- solve(matrixToBeInversed, ...)
  x$setInverse(m)
  m
}
