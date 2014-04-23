## These 2 functions are used to cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverseof the special "matrix" returned by
## the function (makeCacheMatrix) above.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("Getting cached data...")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}





## FYI: Command Prompt Example
# a <- makeCacheMatrix(matrix(1:4, 2))
# cacheSolve(a) --- returns inverse matrix!
# cacheSolve(makeCacheMatrix(matrix(5:8, 2))) -- returns inverse of new matrix


