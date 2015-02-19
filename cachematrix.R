## Package: cachematrix.R
## Author: Gary Cohen
## This file contains a set of functions that allow the caching
## of the inverse of a matrix within a special wrapper object to
## improve the performance time of calculation an inverse repeatedly
## on the same matrix.


## makeCacheMatrix() - This function creates a matrix and also provides methods to return specified 
## properties of the matrix.  By wrapping functions around the actual data structure
## we can hide whether the property values are calculated directly or retrieved from cached
## values stored within the function.

makeCacheMatrix <- function(x = matrix()) {

  ## At first, we do not have the inverse calculated and cached
  inverse <- NULL
  
  ## define function to set the new value of the matrix
  set <- function (y) {
    
    ## Set the new value of the matrix
    x <<- y
    
    ## We have not calculated the inverse yet as we are just setting the matrix
    inverse <<- NULL  
    
  } ## end set()
  
  ## How to retrieve a copy of the matrix
  get <- function() x
  
  ## How to set the inverse of the matrix
  setinverse <- function(inv) inverse <<- inv
  
  ## How to retrieve the inverse of the matrix
  getinverse <- function() inverse
  
  ## Provide a list of all the available functions within this "matrix"
  list(set=set, 
       get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
  
} ## end makeCacheMatrix()


## This function returns the inverse of the special "matrix" option passed in as the
## first parameter.  The special"matrix" is one created with makeCacheMatrix().
## This function will see if the inverse has already been calculated and cached.
## If so, it will return the cached value of the inverse.  If not, it will call solve()
## to calculate the value of the inverse, and will store the inverse in the cache while
## also returning the inverse to the caller.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Attempt to get the inverse value from the cache
  inverse <- x$getinverse()
  
  ## Is the inverse value available from the cache?
  if (!is.null(inverse)) {
    
    message("Getting cached data")
    return (inverse)
    
  } ## end if the inverse value is available from the cache
  
  ## The inverse value was not available from the cache
  ## Now we need to retrieve the matrix data and calculate the inverse directly
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
} ## end cacheSolve()
