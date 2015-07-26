## These pair of functions can cache the inverse of a matrix.
##
##   Matrix inversion is usually a costly computation 
##   and there may be some benefit to caching the inverse of a matrix 
##   rather than computing it repeatedly.
##

## 1.This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setmean <- function(Solve) invm <<- Solve
        getmean <- function() invm
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 2.This function returns the inverse of the special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated (and the matrix has not changed), 
##   then cacheSolve should retrieve the inverse of 'x' from the cache.

cacheSolve <- function(x, ...) {
        invm <- x$getinverse()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setmean(invm)
        invm
}
