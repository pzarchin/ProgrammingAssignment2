## This function creates a matrix that can cache its inverse
## In case the inverse is already been calculated, it will be retrieved from the cache

## Inverse caching

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) invert <<- inverse
        getInv <- function() invert
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)

}


## inverse retrieving

cacheSolve <- function(x, ...) {
                invert <- x$getInv()
        if(!is.null(invert)){
                message("getting cached data")
                return(invert)
        }
        matrx <- x$get()
        invert <- solve(matrx,...)
        x$setInv(invert)
        invert
}
