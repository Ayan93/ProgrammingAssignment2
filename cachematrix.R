# Matrix inverseersion is usually a costly computation and there may be some benefit
# to caching the inverseerse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverseerse of a matrix.

# makeCachedMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverseerse of the matrix
# 4. get the value of inverseerse of the matrix
makeCachedMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverseerse <- function(inverseerse) inverse <<- inverseerse
  getinverseerse <- function() inverse
  list(set=set, get=get, setinverseerse=setinverseerse, getinverseerse=getinverseerse)
}


# The below method will return the inverse of the matrix, it first checks whether the inverse is done.
# If so, it gets the result and skips the computation.
# If not, it computes the inverseerse, sets the value in the cache via setinverseerse function.
# This function assumes that the matrix is always inverseertible.

cachedSolve <- function(x, ...) {
  inverse <- x$getinverseerse()
  if(!is.null(inverse)) {
    message("getting the cached data.")
    return(inverse)
  }
  dataInv <- x$get()
  inverse <- solve(dataInv)
  x$setinverseerse(inverse)
  inverse
}
