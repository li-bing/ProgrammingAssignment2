## caching the inverse of a matrix to make matrix inversion faster
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(y) {
		   x <<- y
		   i <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(inverse) i <<- inverse
	
	getinverse <- function() i
	
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	
	if(!is.null(i)) {
			message("get the cached inverse matrix")
			return(i)
	}

	mat <- x$get()

	i <- solve(mat, ...)

	x$setinverse(i)
	i
}
