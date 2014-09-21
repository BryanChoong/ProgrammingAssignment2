## This code contains a pair of functions ("makeCacheMatrix" and "cacheSolve")
## that will cache the inverse of a matrix

## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      ## creates x as an empty matrix
  I <- NULL                                      ## assigns I as NULL 
  set <- function(y) {                           ## assigns set as a function(y)
    x <<- y                                      ## which assigns x to argument y
    I <<- NULL                                   ## and assigns I to NULL
  }
  get <- function() x                            ## returns the value of x
  setinverse <- function(inverse) I <<- inverse  ## assigns I in makeCacheMatrix to inverse
  getinverse <- function() I                     ## returns the value of I
  list (set = set, get = get                     ## returns a labeled vector of functions set, get,
        setinverse = setinverse,                 ## setinverse,
        getinverse = getinverse)                 ## and getinverse
}


## "cacheSolve" computes the inverse of matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, and the matrix has not changed
## cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  I <- x$getinverse()                   ## gets the inverse from x(if calculated previously)
  if(!is.null(I)) {                     ## if I is not null
    message("getting cached data")      ## prints message "getting cached data"
    return(I)                           ## returns value of I, which was cached
  }
  data <- x$get()                       ## if I is null, assigns data to x from makeCacheMatrix
  I <- solve(data, ...)                 ## calculate the inverse of data
  x$setinverse(I)                       ## set I in x to calculated inverse
  I                                     ## print value of I
}
