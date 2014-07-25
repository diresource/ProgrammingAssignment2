###################
# makeCacheMatrix #
###################
# The function makeCacheMatrix() creates an object that stores 
# a matrix and caches the inverse of that matrix once the inverse
# is created. The object defines four functions used to interact 
# with the original matrix and the invers of the matrix:
#
##  set(),        set the matrix value of the object
##  get(),        get the matrix value of the object
##  setinverse(), set the inverse of the matrix
##  getinverse(), returns the inverse of the matrix or NULL if the 
##                inverse hasn't been calculated yet,
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
  
}


#######################
# cacheSolve-function #
#######################
# The cacheSolve function is the interface for the deveper using the cached 
# inverse matrix functionality to get hold of the invers of the matrix.
# cacheSolve takes a ChacheMatrix (returned by the makeCachedMatrix() 
# function) as argument. The function will check if a previus call to the
# function has caused a calculation of the inverse, if that is the case the 
# cached answer will be return. If no inverse of the matrix can be found, 
# the function will calculate and save an inverse of the matrix. All data used 
# in this function will be read from and written to the CachedMatrix argument.
#
## cacheSolve(), called with a makeCacheMatrix as the argument and returns 
##               the inverse of the matrix
##
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

