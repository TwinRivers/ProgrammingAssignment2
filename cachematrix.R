##******************************************************************************
##
## File Name : cachematrix.R
## Purpose : The following functions illustrate the use of the "<<-" operator 
##           to refer to variables that already exist in some "parent" context
##           -- vairable scope
##
## Background: Matrix inversion is usually a costly computation and their may be  
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. This code has a pair of functions that cache the inverse of a 
## matrix.
##
##  1) makeCacheMatrix is a function that creates a special 'matrix'
##    The input matrix is assumed to be a square invertible matrix.
##  
##  2) This function computes the inverse of the special "matrix" returned by
##     makeCacheMatrix. If the inverse has already been calculated (and the
##     matrix has not changed), then the cachesolve should retrieve the 
##     inverse from the cache.
##  
##******************************************************************************
##
## Function Name : makeCacheMatrix
## Purpose : Creates a special "matrix",as a list of functions
##
## Parameter x: a square matrix
## Returns: list containing the following functions:
## - set the matrix
## - get the matrix
## - set the value of the inverse
## - get the value of the inverse
##
##******************************************************************************



makeCacheMatrix <- function(x = matrix()) {
    ## sets m to NULL: m will store the inverse of a matrix x
    m <- NULL  
    
    ## set the inverse of a matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get: a subfunctions that returns the matrix X
    get <- function() x
    ## setinverse: a subfunctions that saves the inverse of the matrix
    setinverse  <- function(inverse) m <<- inverse
    ## getinverse: a subfunctions that gets the inverse matrix
    getinverse   <- function() m
    # set the subfunctions into a list
    list(set = set, get = get,
         setinverse  = setinverse ,
         getinverse  = getinverse )
}


##******************************************************************************
##
## Function Name : cacheSolve
## Purpose : Computes the inverse of the special "matrix" returned by 
## the function makeCacheMatrix(). If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. Else. the function computes the inverse, stores it
## and returns the value
##
## Parameter x: a square matrix
## Returns: Return a matrix that is the inverse of 'x'
##
##******************************************************************************


cacheSolve <- function(x, ...) {
    ## get the matrix from the object x and put it into the local variable m
    m <- x$getinverse()
    ## Is m null?, if not return M
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Else get the matrix from object x put it into the local variable data
    data <- x$get()
    ## compute the matrix inverse
    m <- solve(data, ...)
    ## store the inverse matrix in the object x
    x$setinverse(m) 
    ## return the inverse matrix
    m
}
