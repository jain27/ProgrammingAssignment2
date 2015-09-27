## This file contains a pair of functions that cache the inverse of a matrix.
## If the contents of a matrix are not changing, the function will cache the value
## of its inverse so that when we need it again, it can be looked up in the cache 
## rather than recomputed. 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  cacheMatrix <- NULL
  
  setMatrix <- function(y) {   # sets the value of matrix as the input matrix
    x <<- y
    cacheMatrix <<- NULL
  }
  
  getMatrix <- function() x #returns the value of the matrix
               
  setinverse <- function(solve) cacheMatrix <<- solve
  getinverse <- function() cacheMatrix
  
  ## create list vector containig a function to set and get the value of the matrix
  ## and to set and get the inverse of the matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  ## this function returns a matrix that is the inverse of 'x'
  
  cacheMatrix <- x$getinverse()     # get the inverse of the matrix if it has been calculated already
  
  if(!is.null(cacheMatrix)) {       # check to see if inverse of matrix x is
    message("getting cached data")  # already stored in the variable cacheMatrix
    return(cacheMatrix)
  }
  
  data <- x$getMatrix()              # get the value of the matrix x
  cacheMatrix <- solve(data, ...)    # calculate the inverse of the matrix
  x$setinverse(cacheMatrix)          # store the value of inverse of matrix x in cacheMatrix
  cacheMatrix                        # return the cache matrix
  
}
