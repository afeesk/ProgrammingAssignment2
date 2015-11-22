### Put comments here that give an overall description of what your functions do
##-------------------------------------------------------------------------------
# PROBLEM:
# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. 
# The following two functions are used to cache the inverse of a matrix.

## Write a short comment describing this function
##-------------------------------------------------
#APPROACH
# makeCacheMatrix creates a list containing a function to that 
#creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {               # set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                # get the value of the matrix
  
  setinverse <- function(inverse) inv <<- inverse  # set the value of inverse of the matrix
  getinverse <- function() inv       # get the value of inverse of the matrix
  
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## Write a short comment describing this function
##--------------------------------------------------
# cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above through the following steps:
# It first checks if the inverse has already been computed. 
# If TRUE, it gets the result from the cache and skips the computation. 
# If not, it computes the inverse, sets the value in the cache using the setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##  Example output from this function:
## > x = rbind(c(1, 2), c(3, 4))
## > mcm = makeCacheMatrix(x)
## > mcm$get()
##       [,1] [,2]
## [1,]     1    2
## [2,]     3    4

## NB: The first calculation of the inverse idicates no cache.
## > cacheSolve(mcm)
##          [,1]  [,2]
## [1,]     -2.0  1.0
## [2,]      1.5 -0.5

## NB: The second run of the "cacheSolve(mcm)" retrieves the result from the cache
## > cacheSolve(mcm)
## getting cached data.
##          [,1]  [,2]
## [1,]     -2.0  1.0
## [2,]      1.5 -0.5
## >
##---------------------------
## USING the solve function:
##---------------------------
##> x = rbind(c(1, 2), c(3, 4))
##> solve(x)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5