## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initialize inverse property
  # set the matrix
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse
  setinverse <- function(inverse) m <<- inverse
  # get the inverse
  getinverse <- function() m
  #return a list
  list(set = set, get = get, 
       setinverse=setinverse,
       getinverse=getinverse)
}


##  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # return inverse if it is already set
  if (!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  # Get the matrix from object
  data <- x$get()
  # find the inverse using matrix multiplication
  m <- solve(data, ...)
  # set the inverse to the object
  x$setinverse(m)
  m
}
