## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## These functions create a matrix and solve it to get the inverse however it also stores the inverse.
## Therefore the inverse is not recalculated over and over again

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve will call the cached value of the inverted matrix, 
## if it exists. If the cached inverted matrix doesn't exist, it will
## invert the matrix itself.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}