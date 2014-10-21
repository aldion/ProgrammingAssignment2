## Contains two functions to calculate and cache the inverse of a matrix :
## makeCacheMatrix: returns an object with the matrix and methods to cache it's inverse.
## cacheSolve: computes and cache the inverse of the object returned by makeCacheMatrix.

# Example usage
# > mymatrix <- matrix(c(4,3, 3,2), nrow=2, ncol=2)
# > cachematrix <- makeCacheMatrix(mymatrix)
# > cacheSolve(cachematrix)
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
#
# > cacheSolve(cachematrix)
# getting cached data
#      [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4


## This function returns a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        # create a special a list containing a function to :
        # set the value of the vector
        # get the value of the vector
        # set the value of the mean
        # get the value of the mean
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## This function assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        
        # check if inverse matrix is cached
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        # compute the inverse of a square matrix
        m <- solve(data, ...)
        
        # cache the result
        x$setinverse(m)
        
        #return inverted matrix
        m
}
