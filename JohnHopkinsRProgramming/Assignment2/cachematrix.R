## This creates a class of objects called makeCacheMatrix that store 
## a matrix and its inverse so that the inverse need not be calculated
## repeatedly

## makeCacheMatrix is a class with functions that store a matrix and 
##its inverse with the object so that it can be used later

makeCacheMatrix <- function(x = matrix()) {
  ## start with an inverse of NULL
  inv <- NULL
  ## sets the matrix we want to cache and resets the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## gets the matrix that is cached
  get <- function() {
    x
  }
  ## stores the inverse of our cached matrix
  setinv <- function(inverse) {
    inv <<- inverse
  }
  ## gets the inverse of our caches matrix
  getinv <- function() {
    inv
  }
  ## Not completely sure why we need this. I think its so R knows
  ## what functions the object has...
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes in makeCacheMatrix objects and returns the
## stored inverse of the matrix. If it has not been calculated before
## it will caculate it and store it as part of the object.

cacheSolve <- function(x, ...) {
  ## Gets the variable inv from makeCacheMatrix object x
  inv <- x$getinv()
  ## if the inverse has already been calculated, return the inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if the inverse has not been calculated, retrieve the matrix
  mat <- x$get()
  ## Calculate inverse
  inverse <- solve(mat)
  ## store inverse
  x$setinv(inverse)
  ## return the inverse
  inverse
}
