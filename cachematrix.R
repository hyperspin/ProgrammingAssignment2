## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function, `makecacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the value of the matrix
#$2.  get the value of the matrix
#3.  set the value of the inverse of teh matrix
#4.  get the value of the inverse of teh matrix

makeCacheMatrix <- function(x = matrix()) {
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



## Write a short comment describing this function
#inverts the matrix if its a new matrix, or teh solution is not available.
# if matrix didn't change, uses the cached solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

