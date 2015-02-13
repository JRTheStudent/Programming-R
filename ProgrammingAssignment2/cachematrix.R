## These functions are intended to be used in combination to cache the inverse
## of a matrix.  The inverted matrix is stored for subsequent setting/retrieval
## in the environment namespace unique to each instantiation of makeCacheMatrix 
## which persists as long as the name assigned to the instantiation of 
## makeCacheMatrix exists.
## Note these functions assume an invertible matrix and do not validate params.
## 
## Example usage:
## > myMatrixCache <- makeCacheMatrix(matrix(1:4,2,2))
## > cacheSolve(myMatrixCache)

## makeCacheMatrix exposes four functions for setting/getting
## a matrix and setting/getting the inverse of a matrix.
## Parameters:
## x: An invertible matrix (defaults to empty matrix)
## Returns:
## List of functions: set(), get(), setinverse(), getinverse() 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
} 

## cacheSolve is intended for use on an object created by makeCacheMatrix and
## calls the functions get(), getinverse() and setinverse() exposed by 
## makeCacheMatrix. If the inverse of the matrix has not already been 
## cached (determined by the getinverse() function), the matrix is retrieved  
## via get(), computed via solve(), stored via setinverse() and returned.
## Parameters:
## x: Object created by makeCacheMatrix (required)
## Returns: 
## Inverse of the matrix passed to makeCacheMatrix or set via the set() 
## function exposed by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
