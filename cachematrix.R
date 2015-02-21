## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #creating getting and setting of local matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #calculate inverted matrix
  setinverted <- function(solve) m <<- solve
  getinverted <- function() m
  
  #create list of functions to return
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   #calculate inverted matrix
   m <- x$getinverted()
   
   #use cached data if available (and print that info)
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   
   #if not cached calculate invertion
   data <- x$get()
   m <- solve(data, ...)
   #call Setinverted function
   x$setinverted(m)
   #return m
   m
}