makeCacheMatrix <- function(x = matrix()) 
{
	## This function creates a special "matrix" object that can cache its inverse.
	cache <- NULL
	
	set <- function(y)
	{
		x <<- y
		cache <<- NULL
	}
	get <- function() x
	setM <- function(solve) cache <<- solve
	getM <- function() cache
	list (set=set, get=get, setM = setM, getM = getM)
}

cacheSolve <- function(x = matrix(), ...) 
{
	## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
	## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
	InvMatrix <- x$getM()
	
	if (is.null(InvMatrix))
	{	
		Matrix <- x$get()
		InvMatrix <- solve(Matrix, ...)
		x$setM (InvMatrix)
		return (InvMatrix)
	}
	else
	{	
		return (InvMatrix)
	}
}
