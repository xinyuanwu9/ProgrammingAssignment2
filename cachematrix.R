## These functions can be used to calculate and store inverse matrix in cache so that we don't
## need to calculate the inverse for the same matrix again.

## makeCasheMatrix function creates a list that store initial matrix and updated the value of
## its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- matrix()
	set <- function(y) {
      	x <<- y
            i <<- matrix()
      }
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function checks if a matrix has its inverse in the cache. If cannot find the inverse,
## it calculates the inverse matrix via solve(), stores it in the cache, and display the value. 

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
      if(!all(is.na(i))) {
      	message("getting cached data")
      	return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      i
}
