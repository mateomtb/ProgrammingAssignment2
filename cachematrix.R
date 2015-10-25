## Put comments here that give an overall description of what your
## functions do

#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly


## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  theinv <- NULL
  set <- function(y) {
    x <<- y
    theinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) theinv <<- inverse
  getinverse <- function() theinv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)


}

## Write a short comment describing this function
# cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  theinv <- x$getinverse()
  if(!is.null(theinv)) {
    message("getting cached data")
    return(theinv)
  }
  data <- x$get()
  theinv <- solve(data, ...)
  x$setinverse(theinv)
  theinv
}
