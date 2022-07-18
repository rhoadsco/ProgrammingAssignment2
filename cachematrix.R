## This file contains two functions that can be used to find the inverse of a
## matrix by cache-ing. 

## makeCacheMatrix sets and gets the value of a vector, then sets and gets the
## inverse of that vector.

makeCacheMatrix <- function( mat = matrix()) {
    inv <- NULL
    set <- function(matrix) {
        mat <<- matrix
        inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is given a matrix and returns its inverse.

cacheSolve <- function(x, ...) {
    mat <- x$getinverse()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- mean(data, ...)
    x$setinverse(mat)
    mat
}
