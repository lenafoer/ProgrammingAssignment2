## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## clearing the cash
  set <- function(y) { ## setting the function
    x <<- y ## setting value
    m <<- NULL ## clearing cash
  }
  get <- function() x ## getting value of matrix
  setinverse <- function(inverse) m <<- inverse ## setting the inverse
  getinverse <- function() m ## getting the inverse
  list(set = set, get = get, ## creeating a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse() ## checking if inverse is already computed
    if(!is.null(m)) { ## if yes, returning cash
      message("getting cached data")
      return(m)
    }
    data <- x$get() ## if we have not inverse, getting matrix
    m <- solve(data, ...) ## ...and calculating inverse
    x$setinverse(m) ## cashing it
    m ## and returning result
}
