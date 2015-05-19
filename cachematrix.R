#######################################################################
#######################################################################
## cacheMatrix.R // Guy Hagen 5/19/2015

#######################################################################
#######################################################################
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


#######################################################################
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


#######################################################################
#######################################################################
### TEST DATA
###
### Loading the above functions and testing with the following commands
### (uncommented first) will produce the following correctly inverted
### and cached matrix result:
###
###      [,1]   [,2]   [,3]  [,4]
### [1,] -0.25 -0.125 -0.125 -1.50
### [2,]  0.00  0.500 -0.500  4.00
### [3,]  0.00  0.000  0.250  0.25
### [4,]  0.00  0.000  0.000  1.00
###
#######################################################################
## m1<- matrix(c(-4,0,0,0,-1,2,0,0,-4,4,4,0,-1,-9,-1,1), nrow=4, ncol=4)
## m2<-makeCacheMatrix(m1)
## cacheSolve(m2)