## The two functions below facilitate the creation of the inverse of (huge) matrices by
## caching the result so it is available for later use without re-calculating.
## This is done with a nifty trick called "lexical scoping", which R uses a lot. In short,
## variables are stored in different environments, and the variables we define in one
## function can not directly be seen, used, or changed by another. Minimise the risk of
## overwriting your cache!

## The makeCacheMatrix() function takes as input an invertible matrix and returns a list
## object containing multiple functions to handle this matrix and its inverse. These
## "sub-functions" are defined in their own environment and cannot be called in the
## global environment.
# In a first step, an empty variable, "invert", is created in which the inverse will be
# stored later.
# Next, the function set() takes as input an invertible matrix and stores it as the
# variable "x", and once more sets the variable "invert" to NULL.
# The get() function returns the matrix stored in "x", so you can access it also in
# other environments.
# The third internal function, setinvert(), sets the variable "invert" to whatever you
# give it as input.
# Last but not least, getinvert() will return the "invert" object (ideally the inverse of
# the input matrix, but this is not written in stone here), making it accessible to
# other environments.
# When all functions are defined, they are returned in the output list.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinvert <- function(inv) invert <<- inv
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## The cacheSolve() function takes as input the output list of makeCacheMatrix() and
## simply returns the inverse of the matrix that was given to makeCacheMatrix().
# First of all, the function calls getinvert() from the list object and writes that to
# a new variable "invert". This is in another environment and will not touch the "invert"
# variable created with makeCacheMatrix(). If the variable exists already (is not NULL),
# it is returned together with a message confirming that the result was already cached.
# Otherwise, the original matrix is taken from the list object with get() and is then
# inverted.
# Only now is the "invert" variable that was created with makeCacheMatrix() changed, using
# setinvert(). This way, the just calculated inverse of the original matrix is chached.
# Finally, the inverse can be returned as output of the cacheSolve() function.

cacheSolve <- function(x, ...) {
  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("Great, data is already cached!")
    return(invert)
  }
  mat <- x$get()
  invert <- solve(mat)
  x$setinvert(invert)
  invert
}
