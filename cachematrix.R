## take the original code, rename mean to inverse
## solve as in getsolve sounds funny in my humble opinion, so using inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return the cached inverse if any, or calculate and cache

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

x <- matrix(c(3, 2, 0, 0, 0, 1, 2, -2, 1), 3, 3)
cachedM <- makeCacheMatrix(x)
cacheSolve(cachedM)
