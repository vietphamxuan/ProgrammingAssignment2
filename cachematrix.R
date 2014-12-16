## Follow the example for caching a vector
## We can use the same logic to cache a matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## Create a set function to save the matrix in memory
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Create a get function to get the matrix from memory
    get <- function() x
    
    ## Create a setinverse function to save the inverse in memory
    setinverse <- function(s) m <<- s
    
    ## Create a getinverse function to get the inverse from memory
    getinverse <- function() m
    
    ## We want all of these functions to be accessed by any caller
    ## we must save all of them in this list as the last statement
    ## which will be returned
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Follow the example for given in the course
## calculate the matrix inverse by using solve function in R

cacheSolve <- function(x = matrix(), ...) {
    ## First look in the cache
    m <- x$getinverse()
    
    ## If we find in the cache, we display a message and return
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }else{
        ## else we display cache missed
        message("missing cached data")
        
        ## get the matrix from in memory cache
        data <- x$get()
        
        ## Using R solve() to calculate the inverse
        m <- solve(data, ...)
        
        ## save the inverse matrix in the cache for later
        x$setinverse(m)
        return(m)
    }
}
