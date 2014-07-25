
## this function will create a special matrix with 4 methods
## set will set the value of the matrix and set the inverse to null
## get will return the matrix value
## setinverse will store the inverse of the matrix in the object
## getinverse will read the inverse from the object

makeCacheMatrix <- function(x = numeric()) {
    inverse <- NULL
    set <- function(value) {
        x <<- value
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## A function that will test if the mean is already calculated 
## it will read it from cache, 
## otherwise it will be calculated and stored in the object cache

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
