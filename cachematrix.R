## The following functions create a matrix and calculates the inverse
## of that matrix.
## The resultant inverse is cached for when that result is required
## for a future input

## makeCacheMatrix function creates a square matrix and calculates
## the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL

      set <- function(y) {
            x <<- y # Assigns x's value in the parent environment
            inv <<- NULL # Resets inv in case a previous iteration of this function exists
      }
      
      get <- function() x # Retrieves x from the parent environment
      
      setinv <- function(solve) inv <<- solve # Sets the inverse of the matrix
      getinv <- function() inv # Retrieves inverse from the parent
      
      
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}


## cacheSolve function retrieves the inverse of the matrix, notifying
## the user when the result is being called from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv() # Retrieves inverse from cache
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
            # If inv is not empty (i.e. not reset on entry of new matrix)
            # user is informed that inv is being retrieved from cache
      }
      data <- x$get() # Retrieves original matrix from cache
      inv <- solve(data, ...) # Solves inverse of original matrix
      x$setinv(inv)
      inv
}
