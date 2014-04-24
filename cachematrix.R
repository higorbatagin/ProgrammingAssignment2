## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix takes an argument x of type matrix and returns a list with 4 wrapped functions
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inv) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getInverse = getInverse)

}


## Write a short comment describing this function
#The input is "special matrix" made from makeCacheMatrix and the output is the inverse matrix
#coming whether from the special matrix's  cache or computation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
