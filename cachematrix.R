## This is a pair of functions that cache the inverse of a square
## invertible matrix

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache
## its inverse
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  ## The following line stores the four functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above
  inv <- x$getinv()
  ## If the inverse has already been calculated (and the matrix
  ## has not changed), then the function retrieves the inverse
  ## from the cache and prints the message
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  X <- x$get()
  inv <- solve(X)
  x$setinv(inv)
  ## The following line returns a matrix that is the inverse of 'X'
  inv
}