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
# I'm *assuming* that because the matrix supplied is always to be invertable, 
# forcing the use of a square matrix is okay... though the instructions
# are not entirely clear

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


## This function computes the inverse of the matrix returned by the 
# makeCacheMatrix function. If the invers has already been calculated, 
# then this function retrieves the inverse from the cache, rather than
# calculating it

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
	m <- solve()	# if inverse is not cached, calculate it
	x$setinverse(m)
	m
}
