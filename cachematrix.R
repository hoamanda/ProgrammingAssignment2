## makeVector creates a special "matrix",
## which is really a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	## 1. set the value of the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	## 2. get the value of the matrix
	get <- function() x

	## 3. set the value of the inverse of the matrix
	setinverse <- function(inverse) m <<- inverse

	## 4. get the value of the inverse of the matrix
	getinverse <- function() m


	## list containing results of four functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Function that returns a matrix that is the inverse of 'x'
## Assumption: x is an invertible square matrix

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
