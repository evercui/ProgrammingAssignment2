## Put comments here that give an overall description of what your
## functions do
## These two functions take the input of a invertible matrix, calculate its inverse and cache the inverse matrix.
## Write a short comment describing this function
## The makeCacheMatrix function takes a invertible matrix and creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	rem <- NULL
	set<-function(y){
		x<<-y
		rem<<-NULL
	}
	get<-function() x
	set_reverse<-function(reverse) rem<<-reverse
	get_reverse<-function()rem
	list(set = set,get=get,set_reverse=set_reverse,get_reverse=get_reverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	m<-x$get_reverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$set_reverse(m)
	m
}

