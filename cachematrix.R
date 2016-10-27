## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inver <<- inverse
  
  getInverse <- function() inver
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not 
## changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  
  mat <- x$get()
  
  inver <- solve(mat, ...)
  
  x$setInverse(inver)
  
  inver
}
