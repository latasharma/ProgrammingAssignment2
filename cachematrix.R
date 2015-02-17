## Create a matrix called makeCacheMatrix that caches its inverse
## Then create a function that computres the inverse of a special 'matrix' returned by makeCacheMatrix. 
##If the inverse has already been calculated then return that value otherwise calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL 
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    matrix(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message ("getting cached data")
        return(m)
    }
    data <- x$get()
    m <-solve (data, ...)
    x$setinverse(m)
    m
}
