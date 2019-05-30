## Inversing a matrix is a memory intensive operation. The following 2 functions provide operations for creating
## the inverse of a matrix, caching it, retrieving it and recreating the cache when the original matrix is updated.


## function "makeCacheMatrix" returns a list object that contains methods to get a matrix, set a matrix, set the inverse of a 
## matrix or get the cached inverse of a matrix. The set function clears out the cached inverse of the matrix so that
## it will be recreated.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function(x) {
    i
  } 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function "cacheSolve" takes a list object that contains methods for getting and setting the inverse of a matrix. It first 
## tries to get the cached inverse of the matrix. If the cached inverse exists, it is returned. If the cached inverse 
## doesn't exists, the function retrieves the original matrix, calculates the inverse, caches it and returns it.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}