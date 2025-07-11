## This file contains two functions:
## 1. makeCacheMatrix: Creates a special matrix object that can cache its inverse.
## 2. cacheSolve: Computes the inverse of the special matrix. If the inverse is already cached,
##    it retrieves the inverse from the cache instead of recalculating it.

## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset the inverse when the matrix is changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return a list of all the above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then it
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # check if inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")  # print message if using cached inverse
    return(inv)
  }
  mat <- x$get()  # get the matrix
  inv <- solve(mat, ...)  # calculate the inverse
  x$setinverse(inv)  # cache the inverse
  inv
}
