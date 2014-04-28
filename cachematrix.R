## Last changed 4/27 19:01
## Define two functions to:
##   - Construct a list of functions that will manage pairs of matrices and their inverse.
##   - Cause the solving/storing or retrieving from 'cache of a matrix inverse.

## makeCacheMatrix will return a list of functions to manage matrix inverses.

makeCacheMatrix <- function(x = matrix()) {     ## function input is a matrix
        m <- NULL                               ## 
        set <- function(y) {                    ## - f(x) to set the initial matrix
                x <<- y                         ##   set the stored initial matrix
                m <<- NULL                      ##   clear the inverse value when a new
                                                ##     matrix is established
        }
        get <- function() x                     ## - f(x) to return the initial matrix
        setinv <- function(solve) m <<- solve   ## - f(x) to set the inverse
        getinv <- function() m                  ## - f(x) to return the inverse
        list(set = set, get = get,              ## construct the list of functions to return
             setinv = setinv,                    
             getinv = getinv)
}


## cacheSolve will provide a matrix inverse by either: 
##   returning the matrix inverse if it exists, otherwise, 
##   solve for the inverse and store it in the cache.

cacheSolve <- function(x, ...) {                ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()                         ##  get the inverse
        if(!is.null(m)) {                       ##  if it exists, return the 'cached' value
                message("getting cached data")  ##
                return(m)                       ##
        }
        data <- x$get()                         ##  otherwise, get the initial matrix
        m <- solve(data, ...)                   ##  solve for the inverse
        x$setinv(m)                             ##  and store it in the cache
        m                                       ## set value of function
}


