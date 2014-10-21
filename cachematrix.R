## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# assume that the matrix supplied is always invertible.

#e.g. 
# m <- matrix(c(4,3, 3,2), nrow=2, ncol=2)
# n <- solve(m)
makeCacheMatrix <- function(x = matrix()) {

  # Computing the inverse of a square matrix can be done with the solve function in R
  
  # This function creates a special "matrix" object that can cache its inverse.
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then cacheSolve should retrieve the inverse from the cache.
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
