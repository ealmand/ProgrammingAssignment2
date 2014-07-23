## This code caches the inverse of a matrix rather than 
## computing it repeatedly

## This function creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        ## initialize inverse variable
        
        set <- function(y) {
        ## define setter function for matrix value
                
                x <<- y
                invx <<- NULL
        
        }
        
        get <- function() x
        ## define getter function for matrix value
        
        setinv <- function(inverse) invx <<- inverse
        ## define inverse setter funciton
        
        getinv <- function(inverse) invx
        ## define inverse getter function
        
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
        ## return list of functions
}
## end makeCacheMatrix

## This function computes the inverse of the special matrix
## returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse 
## from the cache

cacheSolve <- function(x = matrix(), ...) {
         ## Return a matrix that is the inverse of 'x'
        
        invx <- x$getinv()
        ## get cached inverse matrix
        
        if(!is.null(invx)) {
                message("getting cached matrix")
                return(invx)
                ## if inverse is cacehed, print the message
                        
        }
        else {
                message("matrix not cached.... calculating...")
                ## if inverse is not cached, print the message
                
                invx <- solve(x$get())
                ## calculate inverse
                
                x$setinv(invx)
                ## set the inverse on the x object
                
                return(invx)
                ## return the inverse matrix
                        
                
        }
        ## end loop
        
}
## end cacheSolve
## endcachematrix.R