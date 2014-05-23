## cacheSolve(X) takes a square invertible matrix, X, and returns the inverse of the matrix.
## The inverse is cached in the environment via makeCacheMatrix(), and can be retrieved 
## without recomputing by calling cacheSolve() a second time.

## makeCacheMatrix(X) clears any previously cached inverses and creates a matrix-object from
## X, the input square invertible matrix. When cacheSolve() is called, the functions within
## makeCacheMatrix() are called to retrieve the previously cached inverse matrix, I, or to 
## cache a newly computed inverse matrix.

makeCacheMatrix <- function(X = matrix()) {
    I <- NULL
    set <- function() {  
        X <<- y 
        I <<- NULL 
    }                  
    get <- function() X 
    setInverse <- function(inverse) I <<- inverse 
    getInverse <- function() I 
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve computes and returns the inverse of X, a square invertible matrix, and
## caches the inverse using makeCacheMatrix(). If the inverse of X has already been computed,
## the cached inverse is returned.

cacheSolve <- function(X, ...) {
    I <- X$getInverse()
    if(!is.null(I)) { 
        message("getting cached data")
        return(I)
    }
    data <- X$get()
    I <- solve(data, ...)
    X$setInverse(I)
    I
}
