## To cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		i<-NULL
		set<-function(matrix){
			x<<-matrix
			i<<-NULL
			}

		get<-function(){
			x
		}

		setInverse<-function(inverse){
			i<<-inverse
		}

		getInverse<-function(){
			i
		}

		##Return a list of the methods
		list(set=set, get=get,
			setInverse=setInverse,
			getInverse=getInverse)
}


## Compute the inverse of the special matrix
##If the inverse has already been calculated
##(and the matrix has not changed), then the
##"cachesolve" should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	x <- x$getInverse()
	if( !is.null(x) ) {
		message("getting cached data")
		return(x)
	}
	data <- x$get()
	x <- solve(data) %*% data
	x$setInverse(x)
	##Return the matrix
	x
}
