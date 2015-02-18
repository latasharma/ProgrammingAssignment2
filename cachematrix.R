## Create a matrix called makeCacheMatrix that caches its inverse
## Then create a function that computres the inverse of a special 'matrix' returned by makeCacheMatrix. 
##If the inverse has already been calculated then return that value otherwise calculate the inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # this is where the result of the inversion is stored
    set <- function(y) {
        x <<- y
        m <<- NULL # initialises m to NULL
    }
    get <- function() x # return the input matrix
    setinverse <- function(inv) m <<- inv # set the inversed matrix
    getinverse <- function() m # return the inversed matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)# return a list that contains these functions
    
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() # get the inversed matrix from object x
    if (!is.null(m)) {
        message ("getting cached data")
        return(m) # return the calculated inverse matrix
    }
    data <- x$get() # if not, get the matrix object
    m <-solve (data) # solve it( inverse it!)
    x$setinverse(m) # return the solved result.
    m
}
