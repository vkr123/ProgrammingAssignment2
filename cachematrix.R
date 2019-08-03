## The makeCacheMatrix function sets the values for the objects m and x.
## This function returns a list of 4 functions that sets, gets the matrix as well as its inverse
## The cacheSolve is the function that calculates and returns the inverse of the matrix.
## This function further if the inverse is already calculated and if it is null then calculates its inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinv = setinverse,
       getinv = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
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
