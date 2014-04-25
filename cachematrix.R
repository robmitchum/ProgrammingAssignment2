## These functions allow you to cache the inverse of a matrix.

## makeCacheMatrix takes in a matrix and creates a "special" matrix that
## is a list of functions: set, get, setInvert, and getInvert.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinvert <- function(inverse) i <<- inverse
	getinvert <- function() i
	list(set = set, get = get, setinvert = setinvert, getinvert = getinvert )
}


## cacheSolve takes that "special" matrix and checks to see if its inverse
## is already stored in cache. If it is not, it uses the solve function to
## calculate the inverse matrix and store it in cache.

cacheSolve <- function(x, ...) {
    i <- x$getinvert()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvert(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
