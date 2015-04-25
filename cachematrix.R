##
## Caching the Inverse of a Matrix
## Programming Assignment 2 - MOOCS ID: rprog-013.
## 25/04/2015 - CT

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Set a default value for the inverse.
    m <- NULL
    
    # Create attributes for special "matrix" object.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x                       # return the value of x
    setsolve <- function(solve) m <<- solve   # set a new value for the inverse m
    getsolve <- function() m                  # return the value of m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Compute a matrix that is the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    
    # Get the current value of the inverse matrix from the cache.
    m <- x$getsolve()
    
    # If the inverse has already been calculated (and the matrix has not changed), 
    # then the cachesolve should retrieve the inverse from the cache.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # If the inverse has not been calculated yet, then the inverse is computed
    # and the new matrix is stored in the cache.
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
