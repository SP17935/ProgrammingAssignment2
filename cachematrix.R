## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function sets and gets the matrix and then sets and gets the inverse of that matrix, 
## while doing this the function gets the cache 
makeCacheMatrix <- function(x = matrix()) {
                   m <- NULL
                   set <- function(y) {
                          x <<- y
                          m <<- NULL}
                          get <- function() x
                          setInverse <- function() m <<- solve(x)
                          getInverse <- function() m
                          list(set = set,
                               get = get,
                               setInverse = setInverse,
                               getInverse = getInverse)
                   }





## Write a short comment describing this function
##This function is to return the inverse of the matrix that has been created
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
