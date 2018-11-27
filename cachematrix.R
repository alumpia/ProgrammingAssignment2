## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix sets the special matrix structure
# cachesolve calculates the inverse 

## Write a short comment describing this function
# the objective is to create a sort of matrix with the following methods:
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the matrix inverser
#4.  get the value of the matrix inverser
#in the inv vaiable we shall store inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  inv <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function
#in the case that the caché is null, we call the solve method and store the value
#in other case we just return the value stored
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  message("calculating inverse")
  inv
}

#let's test it
#create a square matrix of 50 x 50 elements
#create cachematrix
#call teh solve method through cachesolve
#the first time it calculates the inverse matrix
#the second time it just recalls the value stored
ini.data<-matrix(rnorm(2500), nrow=50, ncol=50)
my.matrix<-makeCacheMatrix(ini.data)
cacheSolve(my.matrix)
cacheSolve(my.matrix)
