## The following functions can be used to create a square matrix and cache its inverse. 
## So the first function makeCacheMatrix has a square invertible matrix as input and creates it as an object,
## and the second one cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## Once the inverse is computed, whenever cacheSolve is called it gets the inverse from cache.

##makeCacheMatrix has an square invertible matrix as input. Creates it as an object.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##cacheSolve evaluates the inverse of the object created by makeCacheMatrix. 
# If it has been already evaluated, it will get from cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
