## Follow the example for caching a vector

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Follow the example for given in the course
## calculate the matrix inverse by using solve function
## in R

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }else{
        message("missing cached data")
        data <- x$get()
        
        ## Using solve to calculate the inverse
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
    }
}
