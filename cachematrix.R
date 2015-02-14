## R Programming Course
## for details see: https://class.coursera.org/rprog-011
##
## Peer assignment solution 2
##  
##
## by Frank M., Sat. 14, Feb 2015

## function makeCacheMatrix
# creates a "special" matrix that caches its inverse for performance reasons
# makeCacheMatrix is used by cacheSolve function (documented below)

makeCacheMatrix <- function(x = matrix()) {
        
        # set inv (inverse) to NULL
        inv <- NULL
        set <- function(y) {
                # assign argument of set function to x
                x <<- y
                # inv is set to NULL again, in case makeCacheMatrix is used with diff matrix
                inv <<- NULL
        }
        
        # return the matrix
        get <- function() x
        
        # set inverse, overrides previous value of inv
        setinv <- function(solve) inv <<- solve
        
        # return inverse
        getinv <- function() inv
        
        # create list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## function cacheSolve
# function computes matrix inverse which is returned by makeCacheMatrix
# 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        # cached data already exists
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # no cached data exists
        data <- x$get()
        
        # invert matrix
        inv <- solve (data, ...)
        # store result with makeCacheMatrix using setinv function
        x$setinv(inv)
        # return result
        inv
        
}

## documented test run:

# > A
# [,1] [,2] [,3]
# [1,]    2    7    6
# [2,]    9    5    1
# [3,]    4    3    8
# > B <- makeCacheMatrix(A)
# > cacheSolve(B)
# [,1]        [,2]        [,3]
# [1,] -0.10277778  0.10555556  0.06388889
#[2,]  0.18888889  0.02222222 -0.14444444
# [3,] -0.01944444 -0.06111111  0.14722222
# > cacheSolve(B)
# getting cached data                       <--- retrieving cached value!
#[,1]        [,2]        [,3]
#[1,] -0.10277778  0.10555556  0.06388889
#[2,]  0.18888889  0.02222222 -0.14444444
#[3,] -0.01944444 -0.06111111  0.14722222
