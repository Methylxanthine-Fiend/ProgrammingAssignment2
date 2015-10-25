makeCacheMatrix <- function(x = matrix()) {
    ##This function creates a special "matrix" object that can cache its inverse.
    inverted <- NULL
    
    setmatrix <-function(y) {
        x <<- y
        inverted <<- NULL
    }
    getmatrix <- function() x
    
    setinverse <- function(solve) inverted <<- solve
    getinverse <- function() inverted
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse, getinverse = getinverse)
       
}
    
cacheSolve <- function(x, ...) { 
    ##Computes the inverse of the matrix returned by makeCacheMatrix above.
    ##If the inverse has already been calculated cacheSolve will return the
    ##cached, inverted matrix; else a freshly calcutated matrix is returned.
    
    inverted <- x$getinverse()
    if(!is.null(inverted)) {
        message('returning cached data')
        return(inverted)
    }
    
    originalmatrix <- x$getmatrix()
    inverse <- solve(originalmatrix)
    x$setinverse(inverse)
    
    inverse
    
}        
