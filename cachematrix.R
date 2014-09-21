## The objective of these two functions is to compute the inverse of a given matrix 
## only if it hasn't been computed before. Otherwise, retrieve it from cache.
##
## makeCacheMatrix returns a list of functions: 
## a. Set (initialize) the value of a matrix
## b. Get the value of a matrix
## c. Given a matrix, compute its inverse
## d. Get the value of the inverse of a matrix
## 
## cacheSolve returns the inverse of a matrix, 
## but only computes it if it hasn't been computed before.
##
## makeCacheMatrix has input the matrix to invert and returns a list of functions
## makeCacheMatrix is able to cache the inverse of a matrix
##
makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL
      set <- function(y) {    
              x <<-y
              xinv <<- NULL
      }
      get <- function() x
      
      setinv <- function(minv) xinv <<- minv
      getinv <- function() xinv
      
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## CacheSolve inputs the list output of makeCacheMatrix
## Compute inverse of matrix only if not computed before
## Otherwise, retrieves from cache using makeCacheMatrix functions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached inverse")
                return(xinv)
        }
        data <-x$get()
        xinv <-solve(data, ...)
        x$setinv(xinv)
        xinv
}
