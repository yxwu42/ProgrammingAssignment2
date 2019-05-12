## Programming Assignment 2:

## the program contains two functions:
## 1. makeCacheMatrix function creates a special matrix object that can cache its inverse
## 2. cacheSolve function computes the iverse of the special matrix returned by makeCacheMatrix.
## If the iverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inv <<- inverse 
        getinverse = function() inv
        list(set=set, 
             get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        
        inv = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinverse(inv)
        
        return(inv)
}
