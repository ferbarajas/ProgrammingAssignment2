## The function "makeCacheMatrix" takes a matrix as an argument that, when assigned to a variable, saves a list with 4 functions based of the original "makeCacheMatrix". This information is then accessed using the "cacheSolve" function, returning the inverse of the original matrix.

## This function takes a matrix as an argument and returns a list with 4 functions that allow the user to set or get the matrix, and to set or get the inverse of the matrix by using the "$" operator.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a variable which value is equal to that of the result of "makeCacheMatrix". It then utilizes the "$" operator to attempt to get the inverse of the matrix. If a solution exists already, the value is taken from cache, in the oposite case, the value is calculated an saved to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
