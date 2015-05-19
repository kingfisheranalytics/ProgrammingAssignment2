## mathematical caching functions for Coursera John Hopkin's R Course, Week 3 Programming Assignment 2
## Guy Hagen 5/19/2015

## The "makeCacheMatrix" function calculates and returns the mathematical inverse of a matrix "x" using the "solve" function, and caches the result ("m").

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)	
}


## This function uses makeCacheMatrix to initialize and cache an inversion of a provided matrix ("x").

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix inversion")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

