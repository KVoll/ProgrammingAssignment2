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
## enclosed environment.                                                      ##
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
    
    ## extract the function list from the matrix 'x'
    func.list <- x[, 1]
    
    ## Get inverse matrix from cache.  If inverse matrix is not bound, m is NULL
    m <- func.list$getinverse()
    
    ## If matrix 'x' is found in enclosed environment, return its inverse matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise, get the values of the matrix passed to makeCacheMatrix
    data <- func.list$get()
    
    ## Calculate the inverse
    m <- solve(data, ...)
    
    ## Store the inverse for matrix 'm' in cached environment.
    func.list$setinverse(m)
    
    ## Return the inverse matrix 'm'
    m
}
