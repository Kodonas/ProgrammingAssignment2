#!/usr/bin/env Rscript

#Donatas Sledzius
#R Programming
#Week 3
#Assignment2

#Caching the inverse of a matrix.

#Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<-y
		inv <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


#Calculates the inverse of the matrix if the inverse has not been created and sets it in cache.
#If the inverse exists, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

#Test with 2x2 matrix with values of 2, 1, 1, 2.
m <- matrix(c(2,1,1,2),2,2)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)