## Following are two functions create the matrix "makeCacheMatrix", 
## then inverse the created matrix "cacheSolve".
## This function creates a special "matrix" object that can cache its inverse .

makeCacheMatrix <- function(x = matrix()) {
          inver <- NULL
          set <- function(y) {
          x <<- y # setting x with y
          inver <<- NULL  # resetting inver variable to NULL
 }
          get <- function() x
          setinverse <- function(inverse) inver <<- inverse 
          getinverse <- function() inver
          list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse) # creation of list
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'

         inver <- x$getinverse()
         if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
  }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
  }   
