## These functions caching the inverse of a matrix rather than computing it repeatedly


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y  #Assign the value y to the the matrx in the parent environment
    m <<- NULL  #Assign the value of NULL to the of inverse of the matrix in the parent environment
  }
  get <- function() x  ## gets the value of the inverse
  setsolve <- function(solve) m <<- solve #calculates the inverse of non-singular matrix via the solve function
  getsolve <- function() m # gets the inverse   
  list(set = set, # gives the name 'set' to the set() function defined above
       get = get,# gives the name 'get' to the get() function defined above
       setsolve = setsolve, # gives the name 'setsolve' to the setsolve() function defined above
       getsolve = getsolve)# gives the name 'getsolve' to the getsolve() function defined above
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  a=makeCacheMatrix(x)
  m <- a$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- a$get()
  m <- solve(data, ...)
  a$setsolve(m)
  m
}## Return a matrix that is the inverse of 'x'

##example
mat <- matrix (c(5,6,7,8), nrow =2, ncol =2)
cacheSolve(mat)
