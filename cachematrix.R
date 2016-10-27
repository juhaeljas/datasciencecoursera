## Caching matrix's inverion data so that it needs to be solved only once 

## Helper functions. Soring the data and functions to same object
makeCacheMatrix <-  function(x = matrix()) {
# define im object (inverted matrix) in makeCacheMatrix function's object environment
    im <- NULL
# define four functions for manipulating and displaying the data

    set <- function(y) {
      x <<- y  # specia operator that can output on object to parent fuction
      im <<- NULL # resetting the im
    }
# this function is used by cachesolve function to introduce the original data to solve 
# function (via data object) for inverting the matrix    
    get <- function() {
      x
    }

# if and when the solve function is used the result must be stored to cachesolve environment's im 
# object where it stays before resetting by makeCacheMatrix and subsequent cachesolve command   
    setsolve <- function(settingsolved) {
      im <<- settingsolved
    }
# cachesolve function uses this for loading the previously inverted matrix if possible
# loads an NULL value if it's not calculated yet.
    getsolve <- function() {
      im
    }
# Output of this function as a list containing 4 functions
    list(
      set = set, 
      get = get, 
      setsolve = setsolve, 
      getsolve = getsolve
    )
}

## calculates the inverted matrix form invertible matrix if needed othervise just print precalculated
## and cached data

cachesolve <- function(x, ...) {
# eighter loads the NULL (first time) of inverted matrix if it's available
   im <- x$getsolve()
# if the inverted is there then just prit it (+ message) by return command and end the function
   if(!is.null(im)) {
     message("getting cached data")
     return(im)
   }
# if no inverted matrix found then store original matrix to data object and do the inversion by
# solve command
   data <- x$get()
   im <- solve(data, ...)
# store the inverted matrix to cache
   x$setsolve(im)
# print just fresly made inverted matrix
   im
}