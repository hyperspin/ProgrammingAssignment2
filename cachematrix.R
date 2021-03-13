## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function, `makecacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set: function to set the value of the matrix
#2.  get: function to get the value of the matrix
#3.  setsolve: function to set the  inverse of the matrix
#4.  getsolve: function to get the  inverse of the matrix

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

## 
#inverts the matrix if its a new matrix.
# calls getsolve function.  uses the cached solution if the rturn value (m) is not NULL
#Prints the message "setting cached data.
# if m is NULL, sets data to the new matrix and computes the inverse..

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

