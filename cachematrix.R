## cachematrix.R defines two functions: makeCacheMatrx() and cacheSolve().
##
## makeCacheMatrix() function creates a special matrix object which supports
## caching of its inverse. 
##
## cacheSolve() function operates on the special matrix objects returned by
## makCacheMatrix() function and returns inverse of the matrix. Internally,
## it caches the inverse for future use.
##
## Demo:
## -----
## > mat1 <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), ncol=3)
## > mat2 <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow=3, ncol=3)
## > mat3 <- matrix(c(3, 2, 0, 0, 0, 1, 2, -2, 1), ncol=3)
## >
## > mat <- makeCacheMatrix(mat1)
## > mat$get()
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## >
## > cacheSolve(mat)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## >
## > cacheSolve(mat)
## Getting cached value of matrix inverse
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > 
## > mat$set(mat2)
## > cacheSolve(mat)
## Getting cached value of matrix inverse
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > 
## > mat$get()
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > 
## > mat$set(mat3)
## > cacheSolve(mat)
##      [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0
## >
## > cacheSolve(mat)
## Getting cached value of matrix inverse
##      [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0
## >
## > mat$get()
##      [,1] [,2] [,3]
## [1,]    3    0    2
## [2,]    2    0   -2
## [3,]    0    1    1
##
## Here, even though mat1 and mat2 are different matrix objects, internally
## they represent the same 3x3 matrix, so even after calling mat$set(mat2), 
## cacheSolve(mat) returned the cached inverse value (which was actually 
## calculated while calling cacheSolve(mat), when mat contained mat1). mat3 
## is not equal to mat2, therefore, after calling mat$set(mat3), cacheSolve()
## had to recalculate the inverse matrix.

## makeCacheMatrix() creates a special matrix which allows caching of the 
## matrix inverse. Returned object is actually a list containing following 
## four methods:
##
## 1. set(y)
## ---------
## It intializes matrix with the given input matrix y and resets the 
## cached inverse to NULL if input matrix is not equal to the current
## matrix.
##
## 2. get()
## --------
## It returns the current matrix object.
##
## 3. setInv(xInv)
## ---------------
## It set the cached value of the inverse to xInv.
## 
## 4. getInv()
## -----------
## It returns the currently cached value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    # Cached value of x inverse
    cachedInv <- NULL
    
    # isEqual() function return TRUE if input matrix y
    # is equal to the matrix x, otherwise it returns FALSE
    isEqual <- function(y) {
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
    }
    
    set <- function(y) {
        # Reset cachedInv only if x and y are different
        if (!isEqual(y)) {
            cachedInv <<- NULL
        }
        x <<- y
    }
    
    get <- function() {
        x
    }
    
    setInv <- function(xInv) {
        cachedInv <<- xInv
    }
    
    getInv <- function() {
        cachedInv
    }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve() function returns the inverse of input matrix x (assuming that
## x is invertible). Internally, it caches the matrix inverse. For a given 
## input matrix, it will calculate the inverse only if cached inverse does not
## exist, otherwise it will just return the cached value.

cacheSolve <- function(x, ...) {
    # Return the cached inverse if inverse has previously been calculated
    cachedInv <- x$getInv()
    if(!is.null(cachedInv)) {
        message("Getting cached value of matrix inverse")
        return(cachedInv)
    }
    
    # Calculate x inverse and cache it for future use
    xInv <- solve(x$get(), ...)
    x$setInv(xInv)
    xInv
}
