
## makeCacheMatrix is a function which takes a square matrix as the input 
## parameter and returns a list containing four functions:
##   set - sets the value of the matrix
##   get - gets the value of the matrix
##   setinverse - sets the value of the matrix inverse
##   getinverse - gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL

	set <- function(y) {
      	x <<- y
            cachedInverse <<- NULL
	}

      get <- function() {
		x
	}

      setinverse <- function(inverse) {
		cachedInverse <<- inverse
	}

      getinverse <- function() {
		cachedInverse
	}

      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## cacheSolve takes a list 'x' created using makeCacheIndex function above, 
## and returns a matrix that is the inverse of the matrix encapsulated in 'x'.

cacheSolve <- function(x, ...) {
	## Check if the inverse has already been created
      m <- x$getinverse()

	## if m isn't null, then we already have the inverse, return that
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }

	## if we get here, need to calculate the inverse
	## so get the matrix, call solve(), store the inverse back into x and return it
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}

