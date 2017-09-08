## Date: 08/09/2017
## Author: Jonathan Cromie
## Description: This file contains 2 functions which will create and cache the 
##              inverse of a square matrix.
##              Caching is simply a term used to describe the storing an object 
##              in memory in order accelerate subsequent access.
##              The way we're going to achieve this is by storing the inversed 
##              matrix object in the parent environment, much like a global 
##              variable.
##              The operator used to do this is "<<-" and is known as a super
##              assignment operator.

## Creates and returns a list containing functions to:
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the inverse value of the matrix
##  - get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inversed matrix by firstly checking if the matrix is stored in
## cache. If not it will perform the inverse operation, i.e,solve, and manually
## store the result in cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        print("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
    
}
