## https://class.coursera.org/rprog-007 assignment #2
## Introducing the <<- operator to cache data. In this case the example
## caches the result of solve(), which is compute intensive.

## These are teh get and set functions for the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, 
             get = get, 
             setmatrix = setmatrix, 
             getmatrix = getmatrix)
}


## This function returns a matrix that is the inverse of x. It checks
## if m is not null and returns the cached data. If m is null, then 
## it calls solve() on the matrix

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
