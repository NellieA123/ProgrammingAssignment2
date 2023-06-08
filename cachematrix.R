## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL ##Marks inverse as Null
  set <- function(y){ ##Creation of set function 
    x <<- y
    inv <<- NULL
  }
  get <- function()x ##Makes the function to get matrix x 
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() {
    inver <-ginv(x)
    inver%*%x ##Function to not get NULL/ obtain the inverse of the matrix
  }
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){ ## Checks if the inverse is NULL
    message("getting cached data")
    return(inv) ##Gives an inverse value
  }
  data <- x$get()
  inv <- solve(data,...) ## Calc the inverse values
  x$setInv(inv)
  inv
}
