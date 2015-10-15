## in providing this solution to PA2, guidance was obtained from:
## https://asitarrives.wordpress.com/2014/10/18/
## understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera
##
## makeCacheMatrix creates a special "matrix", which is really a list containing functions to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse matrix
##   get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
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

## cacheSolve calculates the mean of the special "matrix" created with makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
## cache via the setinverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
