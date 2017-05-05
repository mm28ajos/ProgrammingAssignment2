## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list of functions capable of storing a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function gets the inverse of a matrix stored in a 'cacheMatrix' list.
# If the inverse has been calculated already, the cached inverse is returned.
# If the inverse has not been calculated so far, the inverse is calculted and cached by the list
# for further usage.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
