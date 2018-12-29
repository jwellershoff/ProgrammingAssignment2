## These two functions, "makeCacheMatrix" and "CacheSolve" combined, 
## allows us to create a special object that's able to store a matrix and cache its inverse.

## This particular function is designed to create a special 'matrix' that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        setm <- function(y) {
                x <<- y
                invm <<- NULL
        }
        getm <- function()x                                                    
        setinv <- function(inverse) invm <<- inverse
        getinv <- function() invm 
        list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}

## The "cacheSolve" function allows us to compute the inverse from the special 'matrix' 
## created by "makeCacheMatrix".
## If the inverse has been calculated previously (and the matrix has been left unchanged), 
## it should therefore obtain the inverse from the cache.

cacheSolve <- function(x, ...) {
        invm <- x$getinv()
        if (!is.null(invm)){
                message("getting cached data")
                return(invm)
        }
        matrixdata <- x$getm()
        invm <- solve(matrixdata, ...)
        x$setinv(invm)
        invm
}
