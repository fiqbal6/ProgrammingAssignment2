## Over all, below are functions that, when created, a special object
## will be used to store a matrix, and cache its' reverse. 
## As matrix inversion is a costly computation, this will help cache
## the matrix instead of computing it repeatedly. 

## This function creates a matrix object that can sufficiently cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function () inv
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function below calculates the inverse fo the matrix created 
## above in makeCacheMatrix. If inverse has already been computed, 
## and the matrix remains unchanged, it should get the inverse from 
## the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...)
  x$setInverse(inv)
  inv
}
