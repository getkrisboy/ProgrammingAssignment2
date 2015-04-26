## Matrix inversion is usually a costly computation and for large matrix & inverse operations 
## performance of a repeated matrix inverse computation is best achieved by caching the inverse
## of a matrix rather than to compute it repeatedly.
## The following 2 functions 
## makeCacheMatrix(<matrix>) & cacheSolve(<special-matrix>) works in conjunction by storing a 
## inverse matrix in a cache and retrieving it when wanted, thereby reducing the processing 
## time taken, if otherwise would have taken by recomputing the inverse.

## makeCacheMatrix(<matrix>): This function creates a special "matrix" object that can cache its inverse.
## This function returns a list containing 4 sub functions to:
## (1) set a new matrix
## (2) get the matrix
## (3) set inverse of the matrix
## (4) get inverse of the matrix
makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  
  ## set a new matrix
  setMatrix <- function(newMtx) {
    mtx <<- newMtx
    inv <<- NULL
  }
  
  ## get the matrix that has been set
  getMatrix <- function() mtx
  
  ## set the inverse of the matrix into the cache
  setInverse <- function(inverse) inv <<- inverse
  
  ## get the matrix inverse from the cache
  getInverse <- function() inv
  
  ## store the functions/objects in a list
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(<special-matrix>): This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the inverse from 
## the cache.
cacheSolve <- function(mtx, ...) {

  ## try getting the inverse of 'mtx' from the cache
  inv <- mtx$getInverse()
  
  ## check if the inverse is available in the cache
  if(!is.null(inv)) {
    message("getting matrix inverse from cache")
    return(inv)
  }
  
  ## if you reached this point then matrix inverse is not available, so 
  ## get the matrix whic hwas created earlier in the makeCacheMatrix function
  mtxdata <- mtx$getMatrix()
  
  ## compute the inverse by using the 'solve' function
  inv <- solve(mtxdata)
  
  ## set the inverse into the cache and return it
  mtx$setInverse(inv)
  inv
}
