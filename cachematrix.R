## This couple of functions allow you to calculate and cache the inverse of a
##matrix. Matrix inversion is usually very time consuming. 


## makeCacheMatrix creates an object who stores a matrix and its inverse.
##It contains 4 methods for: setting a matrix (set), setting its inverse
##(setinverse), getting the matrix (get) and getting its inverse (getinverse).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix 'x'. Its input is an object 
##created by makeCacheMatrix. If the inverse has already been calculated, it 
##doesn't calculate it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
