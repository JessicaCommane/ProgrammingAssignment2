## makeCacheMatrix creates a matrix. It contains a list with functions to set the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve calculates the inverse off the matrix created with makeCacheMatrix. It checks to see if this has already been calculated, and if so, retrievves the inverse from the cache. If not, it calculates the inverse and saves this in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}


