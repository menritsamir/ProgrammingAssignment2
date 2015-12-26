## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  ## create a set function which caches a matrix
  set <- function(matrix2) {
    x <<- matrix2
    inverseMatrix <<- NULL
  }
  ## create a get function which gets a matrix from the cache
  get <- function() x
  
  ## Set the inverse matrix as the result of apply solve on x
  setMatrixInverse <- function(solve) inverseMatrix <<- solve
  ## Get the inverse matrix
  getMatrixInverse <- function() inverseMatrix
  
  ## set the new matrix object functions 
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  ## If cached data is not null get cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Else apply the function to inverse the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}

