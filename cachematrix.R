## This function created a special "matrix"
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 	m<-NULL
	set <-function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) m <<- inverse
	getinv <- function() m
	list(set = set, get = get, setinv = setinv,
	     getinv = getinv)
}


## This function ccomputes the inverse of the special "matrix"
## if the inverse exists then the cache retrieves that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
		if (!is.null(m)){
			message("Cached data exists. Getting the same")
			return(m)
		}
		z <- x$get()
		m <- solve(z, ...)
		x$setinv(m)
		m
	
}
