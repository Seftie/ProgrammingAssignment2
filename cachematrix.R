## This function creates a special "matrix" object that can cache its inverse using solve(x)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
 ## Create matrix object which assign value of "y" to x and value of "Null" to inv in parent environment
  creatematrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Retrieve matrix
  getmatrix <- function() x
  #Create inverse matrix
  createinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
#Store functions as a list of objects that can be called in another function
list(creatematrix = creatematrix, getmatrix = getmatrix, createinverse = createinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Retrieve inverse from makeCacheMatrix; if function has  been previously called with a value for x and exists in cache, return cached value
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  ## Calculate new value for inv if a value does not already exist in the cache
  newinverse <- x$getmatrix()
  inv <- solve(newinverse, ...)
  x$createinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}
