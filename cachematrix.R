## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list to set and get value of matrix and it's inverse


makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matInv <<- inverse
  getInverse <- function() matInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve function checks and gets the inverse of matrix if it has been already computed and cached. 
## If it hasn't been computed, it computes inverse and sets value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInverse()
  if(!is.null(matInv)) {
    message("getting cached inverse")
    return(matInv)
  }
  matData <- x$get()
  matInv <- solve(matData)
  x$setInverse(matInv)
  matInv
}
