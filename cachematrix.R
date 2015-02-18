# The included functions are used to generate the inverse of a matrix,
# and to cache the result for later use to prevent recalculation of
# the same inverse in the future.

## makeCacheMatrix()
# Create list containing functions to set/get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve()
# Return the inverse of a matrix, using cached values if available

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    i
}
