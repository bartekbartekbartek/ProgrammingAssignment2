## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverted <- function(solve) m <<- solve
  getinverted <- function() m
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverted()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverted(m)
  m
}





#
# generate matrix, and the inverse of the matrix.
size <- 10 # size of the matrix edge, don't make this too big
mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
mymatrix.inverse <- solve(mymatrix)
#
# now solve the matrix via the cache-method
#
special.matrix   <- makeCacheMatrix(mymatrix)
#
# this should take long, since it's the first go
special.solved.1 <- cacheSolve(special.matrix)
#
# this should be lightning fast
special.solved.2 <- cacheSolve(special.matrix)
#
# check if all solved matrices are identical
identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)
#
# should return TRUE


x <- matrix(1:4, 2, 2)  # x is test 2*2 square matrix
z <- solve(x)   # showing that function solve works .....

xx <- makeCacheMatrix(x)
xxx <- cacheSolve(xx)

#source matrix
x
#if z=xxx than OK
z
xxx
