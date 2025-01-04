## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialization of a null x and cache_mat
  cache_mat <- NULL
  
  ## This is to Globally modify the matrix inverse
  set <- function(y) {
    x <<- y
    cache_mat <<- NULL
  }
  
  ## To return a prompt for the original matrix
  get <- function() x
  
  ## This function saves the inverse in the cache
  setinmx <- function(inv_mat) cache_mat <<- inv_mat
  
  ## To fetch the cache of the inverse matrix
  getinmx <- function() cache_mat
  list(set = set, get = get,
       setinmx = setinmx,
       getinmx = getinmx)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## 'x' being the list of the component functions
  inv_mat <- x$getinmx()
  
  ## The base case of the inverse being in the cache
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  
  ## Else we proceed to calculate the inverse
  ## The matrix is extracted
  mat <- x$get()
  ## Calculating the inverse of 'x' using solve()
  inv_mat <- solve(mat, ...)
  ## Storing the Matrix inverse in the cache
  x$setinmx(inv_mat)
  inv_mat
  ## Return a matrix that is the inverse of 'x'
}
