## Caching the inverse of a matrix
## Creating functions to produce the inverse of a matrix is always much better than
## compute it over and over again. Below is two functions for creating a object to store
## the matrix and caching the inverse of a matrix.

## Create a special object to store the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(inv){
    x <<- inv
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Cache the inverse of the matrix we create in above 'makeCacheMatrix' function.
## If the inverse has been calculated already, the inverse from the cache should be produced.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting inversed matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
