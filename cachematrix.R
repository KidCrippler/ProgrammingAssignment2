## These 2 functions form a construct, which defines a matrix and is able to compute and cache its inverse matrix.
## Caching the inverse matrix could save a big amount of time if your matrix tends to be of big dimension.
## Usage example:

## > mdat <- matrix(c(2, 2, 3, 2), nrow=2, ncol=2)      Creating a simple invertible matrix

## > mdat
## [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > mat <- makeCacheMatrix(mdat)                       Creating our special type of matrix from the input matrix
##                                                      that we've just created
## > cacheSolve(mat)                                    Then computing its inverse matrix, which is shown below
## [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0

## > cacheSolve(mat)                                    Notice that when you "solve" it for the 2nd time, you get a message
## getting cached matrix                                telling you that the value was acquired from the cache, instead of
## [,1] [,2]                                            being computed again (should take a smaller amount of time)
## [1,]   -1  1.5
## [2,]    1 -1.0

## > mat$getinverse()                                   Finally, verifying that the matrix object itself is updated with the
## [,1] [,2]                                            inverse value, and holds it as a member.
## [1,]   -1  1.5
## [2,]    1 -1.0 


## This function creates a matrix whose inverse matrix can be cached (using the function defined immediately after it).
## It does that by defining a set of 4 sub-functions: get, set, getinverse and setinverse, which can be applied on the
## matrix created by them in a simple manner (using the $ operator). Some of these sub-functions are called from the
## 'cacheSolve()' method, thus allowing its inverse matrix to be set automatically when cached, without being explicitly
## told to do that.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets a matrix (x) as an argument, and in addition can get additional arguments that can be passed
## to the 'solve()' method (hence the ...), caches and returns the inverse matrix of x.
## In addition, it stores the computed/cached inverse matrix with the original matrix as a member.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}