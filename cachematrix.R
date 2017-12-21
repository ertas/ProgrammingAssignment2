## This document has 2 functions to implement a matrix-like interface that
## supports the usage of a cached inverse, rather than calculating inverse every
## time. 1st function makeCacheMatrix() generates the matrix object, cacheSolve()
## returns the inverse if it was calculated earlier.


## Create a matrix-like object with 4 setter-getter Functions and which
## stores a cached inverse of the matrix apart from the original matrix x
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(new_m){
        x <<- new_m
        inv <<- NULL
    }
    get <- function() x
    getMyInverse <- function() inv
    setMyInverse <- function(new_inv){
        inv <<- new_inv
    }
    list(set = set, get = get,
         setInverse = setMyInverse,
         getInverse = getMyInverse)
}


## Return inverse of the argument x which is a "matrix" created by makeCacheMatrix()
## function. Either copied from cached value or else computed by solve(). 
cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    ## Return a matrix that is the inverse of 'x'
    if(is.null(inv)){
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
    }else{
        message("getting cached data")
    }
    inv
}
