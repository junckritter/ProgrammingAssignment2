## Functions for computing inverse matrix for given matrix, caching 
## the result for future use within the program

## Special type of matrix, which can save also its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function for computing inverse matrix, caching the result within CacheMatrix

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}
