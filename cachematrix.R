#Function for calculating and storing the inverse of a square  matrix
#called as two separate functions
# the first function gets passed an invertible matrix and creates the area
# the second function gets passed the stored results of the first function
#  the sample code for this looks like
#
#  testmat=matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3)
#
#  zippy<-makeCacheMatrix(testmat)
#  cacheSolve(zippy)
#
#

# Makes a cacheable area to store the Matrix solution

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# Assesses if there is a solution stored
#  if not, calculates the Matrix solution

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
