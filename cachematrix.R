## Caching the Inverse of a Matrix.  Consist of two functions; makeCacheMatrix 
## and cacheSolve, which will create a matrix object and cache its inverse.  
## The inverse is computed by cacheSolve, and if it has already been calculated 
## (and the matrix not changed) then will retrieve the inverse from cache.

## Function makeCacheMatrix creates a matrix object that can cache its inverse.
## solve is used to compute the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
		}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list (set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Function cacheSolve computes the inverse of the matrix created by makeCacheMatrix
## If the inverse has already been calculated (and matrix not changed) then will 
## retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.NULL(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
