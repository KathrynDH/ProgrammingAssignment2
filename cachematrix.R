## These functions cache the inverse of a matix


## This function takes a matrix as input
## sets x equal to the input matrix, sets inverse
## equal to null, and returns list of functions used to
## get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes a list of functions created by
## makeCacheMatrix and returns the cached inverse of 
## the maxtix x if it exists. If it doesn't exist, the 
## function calculates, caches, and returns the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
