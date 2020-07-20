# ProgrammingAssignment2
Caching the Inverse of a Matrix by creating new functions.

## When executing time consuming calculations, it is good to cache the results so that you can search them later instead of calculating them again. So maxtrix reversal is usually expensive, especially when done within a loop.

## This function makeCacheMatrix can compute and create a special object to cache the inverse of an array.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(solveMatrix) inv <<- solveMatrix
      getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function CacheSolve can computes the inverse of the special makeCacheMatrix above. If the inverse has already been calculated, and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setInverse(inv)
      inv      
}
