## Data Science Specialization - John Hopkins University
## R Programming - Programming Assignment #2

## For more information about matrices and the inverse of a matrix,
## I enjoyed this web page https://www.mathsisfun.com/algebra/matrix-inverse.html

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Our assignment is to write a pair of 
## functions that cache the inverse of a matrix.

## makeCacheMatrix - This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve - This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    # Check cache for the inverse matrix from the matrix input
    inv = x$getinv()
    
    if (!is.null(inv)){
        # Return inverse from cache if available
        return(inv)
    } else {
        # Calculate the inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # Set the cache with the calculated inverse
        x$setinv(inv)
        
        return(inv)	    
    }
    
}
