## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMtx <- NULL
  setValMtx <- function(y) {
    x <<- y
    invMtx <<- NULL
  }
  getValMtx <- function() x
  setinverseMtx <- function(inverse) invMtx <<- inverse
  getinverseMtx <- function() invMtx
  list(set1=setValMtx, get2=getValMtx, setinverse3=setinverseMtx, getinverse4=getinverseMtx)
}

## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse4()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get2()
  i <- solve(m, ...)
  x$setinverse3(i)
  i
}


##Test:
m <- makeCacheMatrix(matrix(c(5, 8, 3, 6), c(2, 2)))
n <- cacheSolve(m)

# n
#[,1]       [,2]
#[1,]  1.000000 -0.5000000
#[2,] -1.333333  0.8333333

