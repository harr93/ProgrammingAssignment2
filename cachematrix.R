## This function is used to store a matrix and return the inverse of that matrix
## The inverse is cached to effectively use memory

## This function stores the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL  	## Create a NULL matrix. This will be used to store inverse value.
        set <- function(y) {			## Used to set the value of the matrix. NOTE : Not the return value matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x		## Used to get the value of the matrix
        setinverse <- function(solve) m <<- solve ## Set the inverse of the matrix
        getinverse <- function() m		## Get the value of m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is used to access the matrix

cacheSolve <- function(x, ...) {
 	  m <- x$getinverse() ## Get the cached inverse value
        if(!is.null(m)) { 	## If cached value is not NULL return it
                message("getting cached data")
                return(m)
        }
        data <- x$get()		##If the cached value is NULL access the value of the matrix
        m <- solve(data, ...)		## Calculate the inverse of the matrix
        x$setinverse(m)		## Cache the inverse
        m		## Return the inverse
}
