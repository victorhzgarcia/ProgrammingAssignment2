## The objetive of this assignment is to write a pair of function that cache the inverse of a matrix

## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data previously calculated")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
message("end")
        ## Return a matrix that is the inverse of 'x'
        ## Note: We assume that the matrix is always invertible

