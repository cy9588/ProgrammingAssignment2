# Assigment 2 of Coursera Course titled "R Programming"
# 
# Specifications from assignment
#
# 	Write the following functions:
#	1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#	2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#          If the inverse has already been calculated (and the matrix has not changed), 
#          then the cachesolve should retrieve the inverse from the cache.


# Assignment Note
# 1. "For this assignment, assume that the matrix supplied is always invertible..", hence no need to do any 
#    validation on the input matrix

# Remarks : A template is given in the assignment as demonstration



# ---- About makeCacheMatrix ----
# Four sub functions, they are:
# 	set() : to set the matrix
#	get() : to retrieve the matrix
#	setInv() : to buffer an inverse matrix for future access
#	getInv() : to return an inverse matrix when available, otherwise return NULL

makeCacheMatrix <- function(x = matrix()) {

	xInverse   <- NULL  
	set    <- function(y) {
        	x    <<- y
        	xInv <<- NULL 
      	}
      	get    <- function() x 
      	setInv <- function(invMatrix) xInverse <<- invMatrix 
      	getInv <- function() xInverse 
      	list(set = set, get = get, setInv = setInv, getInv = getInv)
  }


  
# ---- About cacheSolve ----
# try to test if there is inverted matrix buffered, if found, return the inverted matrix, 
# otherwise do the invert operation with function solve() then return
# 
cacheSolve <- function(x, ...) {
        m <- x$getInv() 			# load inverse 
	if(!is.null(m)) { 			# if found, return the matrix in cache 
        	message("getting cached data")
        	return(m) 			# rest of the code in this function skipped
	}
	data <- x$get()
        m <- solve(data) 			# load original matrix and do the invert operation
        x$setInv(m) 				# put the inverse in cache as buffer
        return (m) 				# return the inverse after operation
}
  
