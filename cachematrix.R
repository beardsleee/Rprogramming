## makeCacheMatrix accepts a matrix as a parameter and creates 
## a special "vector" which contains functions to store the
## matrix, store the inverted matrix, retrieve the matrix
## inverse (if it is there) and retrieve the cached matrix.

## This function creates the cached matrix object, with "getters"
## and "setters" for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(v) {
      x <<- v
      m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function performs the solve matrix inverse function on 
## the cacheMatrix object passed in as a parameter.  If the inverse
## is already solved and cached, it returns the cached value. If
## it is not cached, it solves the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if (!is.null(m)) {
          message("getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
