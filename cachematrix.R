## These functions cache inverse (potentially expensive computation) of a matrix

## makeCacheMatrix function accepts a matrix as a parameter and stores it's inverse in "cache". This is like a data bean.

makeCacheMatrix <- function(x = matrix()) {
        inverseX <- NULL
        set <- function(y) {
                x <<- y
                inverseX <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseX <<- inverse
        getInverse <- function() inverseX
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function checks for cached value of the inverse. If it's not set, it calculates the inverse and sets it in the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
