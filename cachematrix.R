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

## Testing
## You may uncomment the below lines as you wish
# x <- matrix(trunc(rnorm(128*128)*100), 64, 64)
# xMatrix <- makeCacheMatrix(x)
# xSolved <- cacheSolve(xMatrix)
# cacheSolve(xMatrix)
# print(xSolved)
## End of testing

## Example output
# [1] "getting cached data"
# [,1]          [,2]          [,3]          [,4]          [,5]          [,6]
# [1,]  6.383915e-04 -4.224930e-04  9.013189e-05  1.008904e-03  4.876540e-04  1.177740e-03
# [2,] -2.684857e-03  3.146259e-03  4.313559e-03 -1.923025e-04 -2.456209e-03 -2.076295e-03
# [3,]  9.653484e-04 -2.715553e-03  7.933232e-04  2.109669e-03  1.292932e-03  4.114644e-03
# [4,] -2.981201e-04  2.544874e-04 -4.198471e-04  1.668689e-03  1.687004e-03  3.077398e-04
# [5,]  1.182467e-04 -2.330996e-03 -1.792270e-03  1.055399e-03  6.124363e-04  1.527677e-03
# [6,] -1.936795e-04 -1.474882e-03 -4.271588e-04 -6.947820e-04  6.792947e-04  1.440347e-03
# [7,] -1.686967e-04 -1.325205e-03 -1.477373e-03 -2.759644e-03  7.758328e-04  1.737416e-04
# [8,]  8.362108e-04  4.891865e-03  3.405006e-04 -6.930141e-04  2.931705e-03 -3.586482e-03
# [9,]  5.117728e-04 -1.593727e-03  3.042437e-04 -2.323823e-04 -2.321342e-03  1.131664e-05
# [10,]  1.916854e-04  9.986129e-04 -1.067611e-03  1.578026e-03  3.082573e-03 -4.974572e-05
# [11,]  6.900640e-04 -2.175847e-03 -1.786433e-03  5.824414e-04  2.627664e-03  1.836163e-03
# [12,]  5.804890e-04 -3.180214e-03 -8.767504e-04  1.334432e-03  1.113721e-03  3.555524e-03
# [13,]  2.314678e-04 -9.971010e-04 -9.790447e-04 -7.410312e-04 -3.724556e-04  2.521291e-04
# [14,]  1.080482e-04  2.581309e-04  2.173484e-03  1.818452e-03 -3.142377e-04  2.296901e-04
# [15,] -5.692378e-04 -2.156334e-03  1.462402e-03  1.371821e-03 -1.566417e-03  1.235911e-03


