## Homework Assignment 3, cache and retreive the inverse of a matrix

## This will create a list which contains methods to get/set the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(mInverse) inverse <<- mInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This method will check if an existing matrix has already computed the inverse and retrieve it.
## If not, it will calculate the inverse and then store the value in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Retrieving cached data")
    return(inverse)
  }
  # not in the cache, calculate it ourselves
  data <- x$get()
  # solve with only 1 param will take the inverse
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

## Testing
# cm <- makeCacheMatrix()
# cm$set(matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE))
# cacheSolve(cm)
