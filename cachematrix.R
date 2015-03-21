## ===============================================================
## cachematrix.R by Low Teck Jan
## Coursera R Programming Coursework
## ===============================================================

## The 'makeCacheMatrix' function takes an input invertible matrix
## and returns a list of functions (set, get, setinv, and getinv)
## for use by the cacheSolve function

makeCacheMatrix <- function(inputmatrix = matrix()) {
  
  # 'if' statements to terminate the function when matrix is not invertible
  if(ncol(inputmatrix) != nrow(inputmatrix)) stop("Matrix is not square")
  if(anyNA(inputmatrix)) stop("Matrix contains NA values")
  if(det(inputmatrix) == 0) stop("Matrix is singular")
  
  # Initiate the cachedinverse matrix
  cachedinverse <- matrix()
  
  # 'set' function changes the input matrix and resets the cached inverse
  set <- function(newmatrix) {
    # Overwrites the old input matrix with the new matrix
    inputmatrix <<- newmatrix
    # Clears the cached inverse matrix for recomputation
    cachedinverse <<- matrix()
  }
  # 'get' function returns the input matrix
  get <- function() inputmatrix
  # 'setinv' function caches the inverse matrix in the parent environment
  setinv <- function(inverse) cachedinverse <<- inverse
  # 'getinv' function returns the cached inverse matrix
  getinv <- function() cachedinverse
  
  # Creates and returns a list containing the four functions above
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## The 'cacheSolve' function either returns the cached inverse matrix
## if already present OR computes the inverse matrix and caches it in
## 'cachedinverse'

cacheSolve <- function(x, ...) {
  
  # Retrieves the cached inverse matrix from the parent environment
  cachedinverse <- x$getinv()
  
  # Checks for a cached inverse matrix and returns it if present
  if(!anyNA(cachedinverse)) {
    message("Retrieving cached inverse matrix")
    return(cachedinverse)
  }
  # If no cached inverse is found, retrieves the input matrix
  data <- x$get()
  # Solves the input matrix and returns the inverse
  cachedinverse <- solve(data,...)
  # Caches the inverse matrix in the parent environment
  x$setinv(cachedinverse)
  # Returns/prints the inverse matrix
  cachedinverse
}