## Functions for caching the inverse of a matrix to avoid expensive recalculation
## Alex Kyllo, 19 Mar 2015

## This function returns an object with setter and getter functions
## for storing a matrix and its inverse

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
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function takes the return value of makeCacheMatrix as a parameter
## and calculates and returns the inverse of the matrix when first called,
## then returns the cached inverse of the matrix on subsequent calls.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
