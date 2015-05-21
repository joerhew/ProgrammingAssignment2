## This series of functions i) creates a matrix object and ii) inverses the matrix object

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {            ##set the value of the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() {             ## get the value of the matrix
                x
        }
        setinv <- function(inv) {       ## set the inverse of the matrix
                i <<- inv
        }
        getinv <- function() {          ## get the inverse of the matrix
                i
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)    ## return list of func's
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {                               ## if the inverse was cached,
                message("getting cached data")
                return(i)                               ## return the cached inverse
        }
        matrix <- x$get()                               ## otherwise, put the matrix in 'matrix'
        i <- solve(matrix)                              ## solve for inverse of the matrix
        x$setinv(i)                                     ## call function to cache the inverse
        i                                               ## return the inverse
}
