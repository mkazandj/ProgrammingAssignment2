## This set of functions allows the user to create cachable matrices whose
## inversions can be cached and which therefore do not need to be computed
## on each call.

## The first function creates a cachable matrix that has get and set closures and 
## whose inverse, initially NULL, has get and set closures. 

makeCacheMatrix <- function(x = matrix()) {
        ## Create a cachable matrix from a matrix object
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function acquires the data from the makeCacheMatrix function and 
## queries whether a matrix inverse is cached. If it does it returns the 
## existing inverse, otherwise it uses the solve function to generate the 
## inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
