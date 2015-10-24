## Per the instructions, this file contains two functions, makeCacheMatrix 
# and cacheSolve. The purpose of the assignment is to teach the passing of 
# variables from function to function using the <<- construct.
#
# I think.
#
# Just like in the example, we have four subfunctions: get, set, getinverse, 
# and setinverse. 
#
# "get" gets the value of the matrix
# "set" sets the value of the matrix
# "getinverse" gets the inverse of the matrix
# "setinverse" sets the inverse of the matrix


## Our first function is makeCacheMatrix. Per the assignment, this function
# "creates a special 'matrix' object that can cache its inverse"


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function provides the inverse of the matrix returned by the 
# makeCacheMatrix function. If the inverse has already been calculated, 
# then this function retrieves the inverse from the cache, rather than
# calculating it.
#
# If the inverse has not yet been calculated, this function calculates 
# it using the R 'solve' command; we have been assured that any matrix 
# passed to it will be invertable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) { # if inverse is cached, retrieve it
		message("retrieving cached data") # changed "getting" to 
						  # "retrieving", see how 
						  # clever I am?
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)	# if inverse is not cached, calculate it
	x$setinverse(m)
	m
}
