## makeCacheMatrix(x)
##      This function takes a matrix as an argument
##      and will return a "cacheable" matrix
##      Used in conjunction with cacheSolve, it is capable
##      of solving for the inverse of a matrix, and then
##      cacheing the result, before returning, upon the first execution.
##      subsequent executions of cacheSolve will return the cached inverse.
##
##  Arguments:  x - shoulmad be an invertable matrix
##
## Example Use:
## 1. For an invertible matrix:
##      > A <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)
## 2. Create a cacheable matrix
##      > A2 <- makeCacheMatrix(A)
## 
## Methods available:
##  <cacheMatrix>$set   - sets a new value for the matrix to be inverted
##  <cacheMatrix>$get   - gets the specified original matrix
##  <cacheMatrix>$setinverse - sets the calculated inverse into a cached location
##  <cacheMatrix>$getinverse - return the cached version of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
    anInverse <- NULL
    matrixChanged <- FALSE
    ## sets the matrix to be inverted
    set <- function(y) {
        x <<- y
        anInverse <<- NULL
        matrixChanged <<- TRUE
    }
    ## return the orignial matrix
    get <- function() x
    ## cache the inverse of the specified matrix
    setinverse <- function(inv) anInverse <<- inv
    ## retrieve the inverse
    getinverse <- function() {
        if (matrixChanged == TRUE) {
            stop("matrix has changed, please rerun cacheSolve")
        }
        anInverse
    }
    ## combine the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve - This function will return the inverse of a matrix
##              It has the added capability of cacheing the resulting
##              inverse after the first execution.  For subsequent 
##              executions of cacheSolve, the cached inverse will 
##              be returned.  For larger matrices,
##              this performance optimization can lead to huge gains
##              depending on the logic of your program
##
## Arguments:   x - is a cacheable matrix made by makeCacheMatrix
##
## Example Use:
## 1. For an invertible matrix:
##      > A <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)
## 2. Create a cacheable matrix
##      > A2 <- makeCacheMatrix(A)
## 3. Execute cacheSolve, to generate and cache the inverse for future executions
##      > cacheSolve(A2)        ## First execution will solve for the inverse and cache it
##       [,1] [,2] [,3]
## [1,]  1.0 -0.5  0.5
## [2,] -4.0  2.5 -3.0
## [3,]  2.5 -1.5  2.0
##
##      > cacheSolve(A2)        ## This is the second exection and should return 
## getting cached data
##       [,1] [,2] [,3]
## [1,]  1.0 -0.5  0.5
## [2,] -4.0  2.5 -3.0
## [3,]  2.5 -1.5  2.0

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    anInverse <- x$getinverse()
    
    ## This checks if the inverse was already calculated before
    ## If so, the cached inverse is returned
    if(!is.null(anInverse)) {
        message("getting cached data")
        return(anInverse)
    }
    
    ## Retrieve the original matrix
    data <- x$get()
    
    ## Solve for the inverse
    anInverse <- solve(data, ...)
    
    ## Cache the inverse
    x$setinverse(anInverse)
    
    ## Return the calculated inverse/cached inverse
    anInverse    
}
