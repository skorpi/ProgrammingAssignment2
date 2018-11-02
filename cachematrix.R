## Put comments here that give an overall description of what your
## functions do

## creates a special vector that can be used to cache an
## inversed matrix.
## example for an invertible matrix: matrix(c(1, -1, 1, 2), nrow = 2, dimnames = list(c("x", "y"), c("a", "b")))
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Used with the vector created with makeCacheMatrix() this function
## makes sure that the inversion fo the matrix is calculated only once
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
