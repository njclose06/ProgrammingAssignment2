## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a data matrix as an input and returns 
## a list of functions that set and provide the inverse of the
## inputted square matrix

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
          x <<- y
          c <<- NULL
  }
  get <- function() x
  setinv <- function(solve) c <<- solve
  getinv <- function() c
  list(set=set, get=get, setinv=setinv,getinv=getinv)
  }



## cacheSolve takes the list generated from makeCacheMatrix
## and returns the inverse of the data matrix provided as 
## the input to makeCacheMatrix. It checks to see if the
## cached variable is there to use; if not it calculates.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
      c <- x$getinv()
      if(!is.null(c)){
        message('getting cached data')
        return(c)
      }
      data <- x$get()
      c<-solve(data, ...)
      x$setinv(c)
      c
      }

