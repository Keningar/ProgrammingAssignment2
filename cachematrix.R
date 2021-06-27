## a function to create the cache object and another to perform the calculation

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    iv <- NULL

    set <- function( matrix ) {
            m <<- matrix
            iv <<- NULL
    }

    get <- function() {
        m
    }

    setInverse <- function(inverse) {
        iv <<- inverse
    }

    getInverse <- function() {
        iv
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



# Perform the calculation
cacheSolve <- function(x, ...) {
    m <- x$getInverse()

 
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()

    m <- solve(data) %*% data

    x$setInverse(m)

    m
}
