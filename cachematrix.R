## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversemat <<- inverse
  getInverse <- function() inversemat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversemat<- x$getInverse()
  if (!is.null(inversemat)) {
    return(inversemat)
  }
  mat <- x$get()
  inversemat <- solve(mat, ...)
  x$setInverse(inversemat)
  inversemat
}
