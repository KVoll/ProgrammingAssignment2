################################################################################
## Author: Kristi Voll                                                        ##
## Date: Oct. 13, 2014                                                        ##
## Coursera CourseID: rprog-008 (R Programming) - Assignment 2                ##
##                                                                            ##
## The purpose of this script is to take advantage of the scoping rules in R, ##
## namely demonstrating how the <<- operator to binds a special object to the ##
## enclosed environment (or cached environment).  One advantage to doing this ##
## is to speed up computing time while performing intensive operations on     ##
## large sets of data.                                                        ##
##                                                                            ##
## In particular, this script caches the inverse of a given matrix within the ##
## enclosed environment using the makeCacheMatrix, and cacheSolve functions.  ##
##                                                                            ##
## Functions:                                                                 ##
##      makeCacheMatrix: Creates a special matrix that can store the inverse  ##
##      of a given matrix in a cached environment.                            ##
##                                                                            ##
##      cacheSolve: Retrieves the bound inverse matrix from cache, or stores  ##
##      an unbound inverse matrix in the enclosed environment.                ##
##                                                                            ##
## NOTE:                                                                      ##
##      "The [operator] <<- ... [causes] a search to be made through parent   ##
##      environments for an existing definition of the variable being         ##
##      assigned.If such a variable is found (and its binding is not          ##
##      locked) then its value is redefined, otherwise assignment takes       ##
##      place in the global environment." - source: help(`<<-`)               ##
################################################################################


## The following function creates a special matrix that can store the inverse 
## matrix of 'x' in a cached environment.  The matrix contains functions that
## implement the following utilities:
##
##      set: stores the values of the matrix 'x' in the enclosed environment by 
##           using <<- operator.
##      get: retrieve the values of the matrix 'x'.
##      setinverse: stores the values of the matrix 'm' (inverse of 'x') in the 
##                  enclosed environment using the <<- operator.
##      getinverse: retrieve the matrix 'm' (inverse of 'x').

makeCacheMatrix <- function(x = matrix()) {
    ## Args:
    ##      x: A square invertable matrix (default: an empty 1x1 matrix). 
    ##
    ## Returns:
    ##      A special matrix containing functions to get and set the matrix and 
    ##      its inverse.
    
    m <- NULL
    
    ## Function that sets the values of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Function that gets the values of the matrix
    get <- function() x
    
    ## Function that sets the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    
    ## Function that gets the inverse of the matrix
    getinverse <- function() m
    
    ## Return the matrix of functions
    as.matrix(list(set=set, get=get, 
                   setinverse=setinverse, 
                   getinverse=getinverse))
}


## The following function computes the inverse of the matrix passed to 
## makeCacheMatrix using the special matrix of functions returned from
## makeCacheMatrix. If the inverse matrix has already been bound in the cached 
## environment, cacheSolve retrieves the inverse from cache, skipping the 
## computation.  Otherwise, if the inverse  matrix is unbound, cacheSolve 
## calculates the inverse and binds it in the cached environment via the 
## 'setinverse' function. 

cacheSolve <- function(x, ...) {
    ## Args:
    ##      x: A special matrix 
    ##
    ## Returns: 
    ##      A matrix indicating the inverse of matrix 'x'.
    
    ## Extract functions from the special matrix 'x' makeCacheMatrix returned
    special.list <- x[, 1]
    
    ## Get the inverse matrix from cache.  If matrix is unbound, m is NULL
    m <- special.list$getinverse()
    
    ## If 'm' is bound in the cached environment, return the matrix from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise, 'm' is unbound. Retrieve the original matrix data using 'get'
    data <- special.list$get()
    
    ## Calculate the inverse
    m <- solve(data, ...)
    
    ## Bind the newly calculated matrix 'm' to the cached environment
    special.list$setinverse(m)
    
    ## Return the matrix 'm'
    m
}
