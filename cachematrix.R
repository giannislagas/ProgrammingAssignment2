##The pair of these functions together cache the inverse of
##a matrix.

##makeCacheMatrix takes as an input a matrix (default empty)
#and returns a list containing functions to 1.set the value of
#a matrix, 2. get the value of a matrix, 3.set the inverse
#4.get the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
		}
	get<- function() x
	setInverse<-function(I) inv<<-I
	getInverse<-function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


##cacheSolve takes as input a list x of type makeCacheMatrix
#and returns the computed inverse or the cached inverse if it 
#has already been computed.

cacheSolve <- function(x) {
		I<-x$getInverse()
		if (!is.null(I)) {
			message("getting cached data")
			return(I)
			}
		M<-x$get()
		I<-solve(M)
		x$setInverse(I)
		I
}

