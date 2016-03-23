## This function creates a special matrix object that can cache its inverse
makeMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) inverse <<- solve
      getInverse <- function() inverse
      list(set = set,get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Computes the inverse of the cacheable matrix returned by makeMatrix()
## If the inverse alread exists and there's no change in the matrix then returns the cached inverse
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      mat <- x$get()
      inverse <- solve(mat, ...)
      x$setInverse(inverse)
      inverse
}

## Used for test
my_matrix <- makeMatrix(matrix(rnorm(9),3,3))
my_matrix$get()
cacheSolve(my_matrix)
