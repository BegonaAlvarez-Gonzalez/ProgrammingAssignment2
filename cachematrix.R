## Put comments here that give an overall description of what your
## functions do
#The function makeCacheMatrix creates a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#The function cacheSolve calculates the inverse of the list created with the
#function makeCacheMatrix. First it checks to see if the inverse has already 
#been calculated. If so, it gets the inverse from the cache and doesn't compute
#it, if not, it calculates the inverse of the matrix and sets its value 
#in the cache via the setinvese function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

