## Below are a pair of functions that cache the inverse of a
## matrix. 

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
			set <- function(y){
					x <<- y
					m <<- NULL
			}
			get <- function() x
			setinverse <- function(inverse) m <<- inverse
			getinverse <- function() m
			list (set=set, get=get, setinverse = setinverse, 
				getinverse = getinverse)
			## Returns special "matrix" 
}

## The cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
        		message("getting cached data")
        		return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
        ## Return the inverse of the matrix
}

## Sample test:
## x <- matrix(1:4,2,2)
## x
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## Make a cache matrix:
## m <- makeCacheMatrix(x)
## m$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## Return inverse of a matrix using cacheSolve:
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## You can validate that this is a correct inversion by going to
## www.matrix.reshish.com/inverse.php