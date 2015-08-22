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
    inversematrix = NULL
    setMatrix = function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    getMatrix = function() {
        x
    }
    setInverse = function(inverse) {
        inversematrix <<- inverse 
    }
    getInverse = function() {
        inversematrix
    }
    list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve - This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    # Check cache for the inverse matrix from the matrix input
    inverse = x$getInverse() ## getinverse() defined in makeCacheMatrix()
    
    if (!is.null(inverse)){
        # Return inverse from cache if available
        return(inverse)
    } else {
        # Calculate the inverse
        matrixdata = x$getMatrix() ## getmatrix() defined in makeCacheMatrix()
        inverse = solve(matrixdata, ...) ## http://www.endmemo.com/program/R/solve.php
        
        # Set the cache with the calculated inverse
        x$setInverse(inverse)
        
        return(inverse)	    
    }
    
}

## References:  Several sources were used in my efforts to research, create and test
## this solution.  In practice, the internet is a valuable resource for finding
## solutions and code strategies.  I successfully completed excessive testing
## of this solution such that I was able to rename variables and to familiarize myself
## with the scoping strategies of each of these functions.
## http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.getenv.html
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.setenv.html
## http://www.endmemo.com/program/R/solve.php
## https://www.mathsisfun.com/algebra/matrix-inverse.html

