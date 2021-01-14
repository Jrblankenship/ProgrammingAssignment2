## makeCacheMatrix works with cacheSolve to store a matrix and the calculated 
## value of its inverse. 
## makeCacheMatrix itself will store a copy of the environment it was called in
## into the designated vector for a defined matrix 'x'. For instance, for
## MyVector <- makeCacheMatrix(x), MyVector will save the matrix data for X.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## the code above sets 'm' to NULL and prepares it to be used later
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## the code above will allow the user to reset 'x' when calling the vector
  ## made initially with makeCacheMatrix. Ie. if you input MyVector$set(x2), 
  ## where x2 is a new matrix. This is without running makeCacheMatrix again.
  ## This also resets any 'm' value in the parent environment to NULL. This will 
  ## allow 'm' to be recalculated if new arguments are set for MyVector$set. 
  get <- function() x
  ## Causes R to retrieve 'x' from the parent environment of makeCacheMatrix
  ## this will be the original matrix
  setinverse <- function(inverse) m <<- inverse
  ## this is the setter for 'm'
  ##assigns the inverse argument to the 'm' value in the parent environment
  getinverse <- function() m
  ## This is the getter for the inverse value. Calling it will cause R to 
  ## retrieve 'm' from the parent environment
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ## this code assigns set, get, setinverse, and get inverse as an element 
  ## within a list. Basically allowing use to use '$' to access the functions 
  ## by name.
}
## Once cacheSolve is run, MyVector$get will return the vector and 
## MyVector$getinverse will return the inverse of the vector.

## ------------------------ cacheSolve below ---------------------

## cacheSolve requires makeCacheMatrix in order to function. The role of 
## cacheSolve is to first "populate" the values in the vector created by
## makeCacheMatrix as well as calculate the inverse of x from the vector. Once
## run, it can also be used to extract the inverse of x that was stored in
## the vector. For example cacheSolve(MyVector) will give the inverse of matrix 
## 'x', if called again, it will not need to recalculate the inverse, but will 
## retrieve it from "storage" in MyVector if called again, unless new arguments
## are used with MyVector).

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## this retrieves the value of 'm' stored in the makeCacheMatrix vector 
  ## (ie. from MyVector)
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  ## if there is a value calculated for 'm' stored in MyVector (thus not NA), 
  ## it will tell the user that it is returning stored data with a message. If
  ## the value is not stored, it will move forward.
  }
  data <- x$get()
  ## this pulls in the matrix stored in the vector made by makeCacheMatrix (ie
  ## stored in MyVector).
  m <- solve(data, ...)
  ## this will calculate the inverse of the matrix stored in MyVector
  x$setinverse(m)
  ## this will set the value of 'm' calculated above to the parent environment
  m
  ## Return a matrix that is the inverse of 'x'
}
