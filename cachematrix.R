## The makeCacheMatrix and cacheSolve functions compute and 
## cache the inverse of a matrix, retrieving the inverse from
## the cache if it has already been stored there (and if the
## matrix itself hasn't changed).  

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
        ## creates 4 functions to set and get the values of
        ## a matrix (m) and its inverse (inv)
  
        inv <- NULL			
        set <- function(m1) {
                m <<- m1
                inv <<- NULL		
        }
        get <- function() m		
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
  
        ## returns a list of the 4 functions
  
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve fuction computes in the inverse of the special
## "matrix" object returned by makeCacheMatrix above.  If the inverse
## has already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(m, ...) {
        
        ## retrieves cached value of the inverse of m
        
        inv <- m$getinverse()
        
        ## returns a matrix that is the inverse of m if its cached value 
        ## is not NULL, along with the message "getting cached data"
        
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if the cached value of the inverse of m is NULL,
        ## calculates the inverse of m and returns it
        
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv
}
