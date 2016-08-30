## this function creates a special list which contains a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inversion
## 4. get the inversion
makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  ## 1. set the matrix
  set <- function(y) {
    ##useing operator "<<-" to assign the value to the object in an environment
    ##other than current environment
    x <<- y
    invert <<- NULL
  }
  ## 2. get the matrix
  get <- function() x
  ## 3. set the inversion
  setinvert <- function(solve) invert <<- solve
  ## 4. get the inversion
  getinvert <- function() invert
  ## special list; output of function
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## this function calculate inversion of the matrix using special list 
## created by above function
cacheSolve <- function(x, ...) {
  invert <- x$getinvert()
  ##check to see if inversion has already been calculated
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  ## otherwise calculate the inversion
  invert <- solve(data, ...)
  ## set inversion matrix in the cache via the seinvert function
  x$setinvert(invert)
  invert
}
