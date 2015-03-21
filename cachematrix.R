## This function caches the inverse of a matrix, which may be needed 
## repeatedly but is usually costly to cmopute

## There are two functions:
## 1. makeCacheMatrix: creates a special "matrix" object that can 
##    cache its inverse
## 2. cacheSolve: computes the inverse of the special "matrix" returned 
##    by makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
        ##This function set, get the matrix, and then set, get its
        ##inverse, and returns a list, which is used as the input
        ##to function cacheSolve()
        
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        set_inv = function(inverse) inv <<- inverse
        get_inv = function() inv
        list(set=set, get=get,set_inv=set_inv,get_inv=get_inv)
}


## This function cacheSolve checks if the inverse of the matrix has been computed or
## or not. If not, it will compute the inverse and sets the value of the
## inverse with setinv function. If yes, it will just get the result from
## the cache. The ruturns of this function cacheSolve is the inverse of the 
## matrix x, originally input to function makeCacheMatrix.

## For the moment, the matrix is assumed invertible.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        
        # if already computed, return it
        if (!is.null(inv)) {
                message("obtained from cache") 
                return(inv)
        }
        
        # if not, calculate the inverse and return it
        mtrx <- x$get()
        inv <- solve(mtrx)
        x$set_inv(inv)
        inv        
}

