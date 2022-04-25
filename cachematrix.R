# A function taht creates a special "matrix" object that can cache its inverse,
# and a function computes the inverse of the created matrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

## The first function creates a special "matrix", which is really a list containing a function to

## set the value of the matrix,
## get the value of the matrix,
## cache the inverse,
## get the inverse

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


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}


mat <- makeCacheMatrix(matrix(c(1,-5, 20, 0, 1 ,-4, 0, 0, 1), nrow=3, ncol=3))
mat$get()
mat$getinverse()

cacheSolve(mat)

cacheSolve(mat)


