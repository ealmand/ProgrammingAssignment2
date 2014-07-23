## This code caches the inverse of a matrix rather than 
## computing it repeatedly

## This function creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) invx <<- inverse
        getinv <- function(inverse) invx
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)

}


## This function computes the inverse of the special matrix
## returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse 
## from the cache

cacheSolve <- function(x = matrix(), ...) {
         ## Return a matrix that is the inverse of 'x'
        
        invx <- x$getmatrix()
        if(!is.null(invx)) {
                        message("getting cached matrix")
                        return(invx)
                        
        }
        else {
                invx <- solve(x$get())
                x$setinv(invx)
                return(invx)
                
        }
        ## end loop
        
}
## end cachematrix.R