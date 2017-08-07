## A pair of functions that cache the inverse of a matrix

## This function creates a special matrix objext that can cache it's inverse.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL   ##Initializes the inverse property
  set <- function(matrix) {  
    m <<- matrix
    i <<- NULL
  } ##Method to sets the matrix function
  
  get <- function() {m} ##Method to get the matrix
  
  setInverse <- function(inverse) {i <<- inverse} ##Sets the inverse of the matrix
  
  getInverse <- function() {i} ##Gets the inverse of the matrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ##Returns a list of the functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m) ) { ## "A is not null" check is done to see if we have cache. 
    message("Getting cached data!")
    return(m) ## This will exit the function and stop execution and will just return the inverse if it's already set
  }
  
  data <- x$get() ## Get the matrix from our object
  
  m <- solve(data) %*% data ## Calculate the inverse using matrix multiplication
  
  x$setInverse(m) ## Set the inverse to the object
  
  m ## Return the matrix
  
}
