## Cache the inverse of a matrix

## Create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(i) im <<- i
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the 
## special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Get the inverse matrix, if it exists, from the cache
  ## and return it
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  ## If the inverse has not yet been calculated
  ## Get the matrix
  m <- x$get()
  ## Calculate the inverse (assumes square matrix)
  im <- solve(m, ...)
  ## Cache the inverse matrics
  x$setinverse(im)
  ## Return the inverse matrix
  im
}
