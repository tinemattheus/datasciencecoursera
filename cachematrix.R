## This script contains 2 functions:
##      1. makeCacheMatrix creates a special "matrix" object that can cache its inverse
##      2. cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Input x: an invertible matrix
        ## Output:  list containing 4 functions:
        ##              1. set the values of a matrix
        ##              2. get the values of a matrix
        ##              3. set the inverse of a matrix
        ##              4. get the inverse of a matrix

        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function returns the inverse of input 'x' which is
## the output of makeCacheMatrix().
## It computes the inverse of 'x' or retrieves the inverse
## from the cache if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
       
        ## Input x: output of makeCacheMatrix()
        ## Output:  inverse of the input matrix of makeCacheMatrix()

        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
