## function makeCacheMatrix store four functions:
## set sets the value of matrix
## get returns the value of matrix
## setInverse calculates inverse of the matrix
## getInverse returns inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Get inverse of the input matrix. If inverse exists then return cached inverse.
## If inverse is NULL, calculates the inverse using solve(),
## caches and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m)
  x$setInverse(inv)
  inv
}
