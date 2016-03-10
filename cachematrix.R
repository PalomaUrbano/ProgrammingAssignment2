## This R file contains 2 main functions to cache 
## the inverse of a matrix versus compute it everytime the function is executed
## The functions are "makeCacheMatrix" and "cacheSolve"

## "makeCahcheMatrix" accepts one parameter which is a square matrix
## This functions calculates the inverse of the matrix and caches it
## Example on how to instanciate this function: x <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    
  }
  get <-function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list (set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function that resolves if the matrix is already cached and print it out
## The Inverse matrix will not be calculated if already is available in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {

  ## If the inverse matrix is already cached it goes through this if statement
  ## and returns the following message
    
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



