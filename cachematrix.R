## These functions cache the inverse of a matrix in order to avoid
## repeated computation which is costly and time-consuming.


## This function creates a special "matrix" which returns a list of 
## functions to (1) set the value of the matrix (2) get the value of
## the matrix (3) set the value of the inverse (4) get the value of the
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(matinv) inv <<- matinv
  getinv = function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}

## This function takes one argument which is the special matrix returned
## by makeCacheMatrix. If the inverse is already calculated, it is 
## retrieved from the cache and returned. Otherwise, the inverse is 
## computed.
cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        mat_data = x$get()
        inv = solve(mat_data, ...)
        x$setinv(inv)
        inv
}

