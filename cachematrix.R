## makeCacheMatrix creates a matrix consisting of set, get, setInverse, and getInverse.
## cacheSolve calculates the inverse of a matrix, unless the inverse is already in
## the cache.  If it is, it skips the calculation and simply retrieves the inverse
## from the cache

## creates a matrix consisting of set, get, setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## computes inverse of matrix unless inverse is already in cache

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}