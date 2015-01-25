## This function leverages the method used to cache the mean of a vector to instead
## calculate the inverse of a matrix and then used the cached values.

## The functions below assume a square invertible matrix.

## These functions cache the inverse of a matrix so once it has been computed, 
## the value can be reused from the saved value instead of recomputed.
## Of course if the matrix has changed, then the value has to be recomputed.

# The first function, makeCacheMatrix creates a special "matrix", which is really a 
# list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(i) inverse <<- i
        get_inverse <- function() inverse
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve retrieves the inverse from the cache.
## So, if the matrix not changed, this returns the inverse of the matrix much faster.
cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting inverse of matrix from cache")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$set_invese(inverse)
        inverse
}
