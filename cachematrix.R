## This pair of functions uses R's scoping rules to store a matrix
## and its inverse, saving processing time in case the inverse is
## needed multiple times.

## makeCacheMatrix provides storage, setters, and getters
## for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setSolve <- function(inv) inverse <<- inv
	getSolve <- function() inverse
	list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve
	)
}


## cacheSolve stores a matrix's inverse in makeCacheMatrix, if it
## doesn't already exist

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getSolve()
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setSolve(inv)
	inv
}
