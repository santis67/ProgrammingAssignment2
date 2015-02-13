## OBJECTIVE:
## To avoid costly (time-consuming) computations on complex operations 
## (for instance, repeatedly computing a matrix inverse) by using a cache.
##
## IMPLEMENTATION:
## Two functions are implemented: makeCacheMatrix and cacheSolve
##      1. makeCacheMatrix: This function creates a "matrix" object that
##                      can cache its inverse. You can set and get a matrix, 
##                      and set and get the inverse. The function returns a list
##
##      2. cacheSolve: This function is called from makeaCacheMatrix, and 
##                      computes the inverse of the "matrix" provided within
##                      makeCacheMatrix. In the case that the inverse has 
##                      already been calculated (and the matrix has not 
##                      changed), then function returns the inverse from cache.
##


## makeCacheMatrix: function that accepts a matrix argument and returns a list. 
## The list contains functions to set and get a matrix, and set and get the 
## matrix inverse
## Example: 
##      amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##      amatrix$get()

makeCacheMatrix <- function(x = matrix()) {

        # Set the container for the inverse to NULL
        im <- NULL
        
        # set function that assigns the value of the matrix to the parent
        # environment of makeCacheMatrix and resets to NULL the inverse matrix
        # variable in the environment within makeCacheMatrix
        set <- function(y) {
                x <<- y
                im <<- NULL                
        }
        
        # get function to supply the original matrix
        get <- function() x
        
        # setinverse function to set the inverse matrix value.
        # The cacheSolve environment, established during inverse calculation, 
        # is compared to the parent environment; this is to assure that 
        # the inverse is not changed by direct call of setinverse; the call 
        # should come from cacheSolve
        setinverse <- function(inv) {
                # Compare the environments from:
                #       - the calling function via parent.frame()
                #       - the environment established in the cache
                if (identical(cacheSolveEnv, parent.frame())) { 
                        # A match of environments, so its all good to change the 
                        # inverse
                        im <<- inv
                } else {
                        # The setinverse function was called directly, so reset
                        # to NULL; user has to re-calculate the inverse.
                        # A message will alert of the issue
                        wmsg <- c("No direct change of the inverse allowed; use 
                                makeCachematrix()")
                        warning(wmsg,noBreaks. = TRUE)
                        
                        # Here I can set to NULL, and force the user to re-do 
                        # the calculation
                        # im <<- NULL
                        
                        # But, maybe it was not intentional; so no change to the
                        # inverse, keep moving on with a warning...
                }
        }
        
        # getinverse function that returns the matrix inverse cached in the 
        # makeCacheMatrix environment
        getinverse <- function() im
        
        # list to establish the cache structure for access and storage
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
                
}


## cacheSolve: function that accepts a matrix, and returns the matrix inverse
## The function assigns to a variable the environment in which the inverse was
## computed; this is validated in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the actual value of the inverse
        im <- x$getinverse()
        
        # Check if the inverse exists and return it from cache
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        # If the inverse is not in the cache, get the matrix to solve
        mdata <- x$get()
        
        # Solve for the inverse
        # (Note: solve(a,b,...) can solve the equation a %*% x = b for x, 
        # where b can be either a vector or a matrix) 
        im <- solve(mdata)
        
        # Get the current environment and add it to a variable. This variable
        # will be used to avoid direct use of setinverse
        cacheSolveEnv <<- environment()
        
        # Set the inverse into the cache and return the inverse matrix
        x$setinverse(im)
        im
        
}
