## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. The two function
## below are written to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	        
        ## set the value of matrix
                inv <- NULL
        set <- function( matrix ) {
                x <<- matrix
                inv <<- NULL
        }
        ## get the matrix
        get <- function() {
               x ## Return the matrix
        }
        ## Set the inverse of the matrix
        setInv <- function(inverse) {
                inv <<- inverse
        }
        ## Get the inverse of the matrix
        getInv <- function() {
                        inv ## The inverse property
        }
        ## Back to a list of the methods
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The following function returns the inverse of the special "matrix",  
## returned by makeCacheMatrix above. It first checks if the inverse 
## has already been computed. If so, it gets the result and skips 
## the computation. If not, it computes the inverse, sets the value 
## in the cache via setInv function above.

## This function assumes that the matrix is square and invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getInv()
	        if(!is.null(inv)) {
	                message("getting cached data")
	                return(inv)
	        }
	        inv <- solve(x$get())
	        x$setInv(inv)
        return(inv)
}
