## Programming assignment by Polina Filipova (GitHub user VoidHamlet)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Set the matrix (x = matrix()).
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the matrix.
        
        get <- function() x
        
        ## Set the inverse of "solve" and get the inverse of "solve", for the matrix.
        
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## Return a list with everything we have declared so far.
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## This function is used in conjuction with makeCacheMatrix,
        ## ergo we are getting the inverse of a matrix (x = matrix()).
        
        m <- x$getinverse()
        
        ## Do we already have this cached? If yes, let's return it.
        
        if(!is.null(m)) {
                message("Matrix already cached. Loading data.")
                return(m)
        }
        
        ## If we do not have it cached, let's get it.
        
        data <- x$get()
        
        ## Solve the matrix.
        
        m <- solve(data, ...)
        
        ## Set the inverse of the "solve" calculation and return it. 
        
        x$setinverse(m)
        m
}