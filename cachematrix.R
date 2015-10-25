## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
