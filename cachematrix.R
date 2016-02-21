## Put comments here that give an overall description of what your
## functions do
## Write a pair of functions that cache the inverse of a matrix.
## And Write an R function that is able to cache potentially time-consuming computations.

## Write a short comment describing this function
## makeCacheMatrix description:
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            ## use `<<-` to assign a value to an object in an environment 
            ## different from his current environment. 
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse 
      getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
## cacheSolve description:
## Return a matrix that is the inverse of 'x'
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      ## if the inverse has already been calculated
      if (!is.null(inv)){
            ## get the inverse from the cache and skips the computation. 
            message("getting cached data")
            return(inv)
      }
      
      ## otherwise, determines the inverse 
      mat.data <- x$get()
      inv <- solve(mat.data, ...)
      
      ## sets the inverse Matrix with the setinv function.
      x$setinv(inv)
      
      return(inv)
}

## This function is able to cache potential time-consoming computations.
cacheTimec<- function(mat){
      ## @mat: an invertible matrix
      
      Minv <- makeCacheMatrix(mat)
      
      start.time <- Sys.time()
      cacheSolve(Minv)
      Timediff <- Sys.time() - start.time
      print(Timediff)
      
      start.time <- Sys.time()
      cacheSolve(Minv)
      Timediff <- Sys.time() - start.time
      print(Timediff)
}

## running my program
r <- runif(10000, 1, 10)
mat1 <- matrix(r, nrow=100, ncol=100)
cacheTimec(mat1)

