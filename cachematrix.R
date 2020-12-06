## This function cache the inversed matrix stored

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmean(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
## Please feel free to use the below example to test the accuracy of the above functions.
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
True_result <- solve(A)
myMatrix <- makeCacheMatrix(A)
Caclulated_result=cacheSolve(myMatrix)
identical(True_result, Caclulated_result)
