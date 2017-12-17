## These two functions allow us to use the cached value of the inverse of a matrix
## rather than recalculating the inverse each time

## makeCacheMatrix computes the inverse of a matrix object and caches it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv = function(solve) m <<- solve
  getinv = function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment descr

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmean(m)
  m
}
