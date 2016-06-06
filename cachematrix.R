
makeCacheMatrix <- function(x = matrix()) {
 
  inverse <- NULL
  ## initialization of matrix and assigning inverse as NULL.
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(a) inverse <<- a ## initialize setInverse 
  
  getInverse <- function() inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) ## set list of methods
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
 
    ## Return a matrix that is the inverse of 'x' If Inverse already stored in cache then print the retrieved matrix else initialize and set the inverse matrix.
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
      message("get cached data")
      return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
         ## Return a matrix that is the inverse of 'x'
}
