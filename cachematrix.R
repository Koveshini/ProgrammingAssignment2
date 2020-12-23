## A pair of functions that cache the inverse of a matrix

## The first function, "makeCacheMatrix" creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                  ## Initializing inverse as NULL
        set <- function(y) {         ## Method to set the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x                           ## Method to get the matrix
        setinv <- function(inverse) inv <<- inverse   ## Method to set the inverse of the matrix
        getinv <- function() inv                      ## Method to get the inverse of the matrix
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)   ## Return a list of the methods
}


## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix"
## If the inverse has already been calculated, then "cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {     
        inv <- x$getinv()            ## Return a matrix that is the inverse of "x"
        if(!is.null(inv)) {          ## Check if inverse is NULL
                message("getting cached data")   ## Just return the inverse if it is already set
                return(inv)          ## Returns inverse value
        }
        data <- x$get()
        inv <- solve(data, ...)      ## Calculates inverse value
        x$setinv(inv)
        inv                          ## Return a matrix that is the inverse of "x"
}
