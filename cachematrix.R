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
