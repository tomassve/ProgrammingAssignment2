# The function cacheSolve(x) returns a matrix inverse of the special matrix
# object x. If cacheSolve is repeatedly called with the same special matrix 
# object a cached version of the inverse matrix is returned and thus costly
# computations are saved.
#
# The special matrix object x is created by calling makeCacheMatrix(m), with
# a numeric or complex matrix.
#


# makeCacheMatrix returns a special "matrix" object as a list of functions:
#   $ set     Set matrix of the object
#   $ get     Get matrix
#   $ setinv  Set inverse matrix of the object
#   $ getinv  Get inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  cachedInv <- NULL
  
  #   $set()     Set matrix of the object
  set <- function(y) {
    x <<- y
    cachedInv <<- NULL
  }
  
  #   $get()     Get matrix
  get <- function() x
  
  #   $setinv()  Set inverse matrix of the object
  setinv <- function(inv) cachedInv <<- inv
  
  #   $getinv()  Get inverse matrix
  getinv <- function() cachedInv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve(x) returns a inverse matrix of the special matrix object x
cacheSolve <- function(x, ...) {
  # Probe for cached version
  cachedInv <- x$getinv()
  if(!is.null(cachedInv)) {
    message("getting cached data")
    return(cachedInv)
  }
  # No cached version. Get matrix and compute inverse
  data <- x$get()
  cachedInv <- solve(data, ...)
  x$setinv(cachedInv)
  cachedInv
}

# testCacheSolve test the code and returns an identity matrix of order 2
testCacheSolve <- function() {
  
  # Create matrix object
  m1<-makeCacheMatrix(matrix(c(2,1,1,2),2,2))
  
  # Calculate inverse matrix
  invm1<-cacheSolve(m1)
  
  # Access cached inverse matrix
  m1$get() %*% cacheSolve(m1)
  
}
