## The following functions allow us to cache the inverses of matrices such that
## they can be computed and retrieved easily  

## makeCacheMatrix is a function that stores a list of four functions (set, get, setsolve, getsolve) to
## set the value of the matrix x from its local environment to the global environment
## get returns the value of the matrix stored
## setsolve then stores the value of the input into the 'makeCacheMatrix' function
## and getsolve then returns the stored value

  makeCacheMatrix <- function(x = matrix()) {
                      m <- NULL
                      set <- function(y) {
                              x <<- y
                              m <<- NULL
                            }
                      get <- function() x
                      setsolve <- function(solve) m <<- solve
                      getsolve <- function() m
                      list(set = set, get = get,
                          setsolve = setsolve,
                          getsolve = getsolve)
                      }   


## Computes the inverse of the matrix created by 'makeCacheMatrix' above. 
## It searches the parent environments to see if the inverse has already been computed and stored previously
## If so, it retrieves the inverse from the cache with return(m) and saves time by skipping recomputation.
## If not, it computes the inverse of the matrix and stores the calculated value in the cache.

  cacheSolve <- function(x, ...) {
                  m <- x$getsolve()
                  if(!is.null(m)) {
                      message("getting cached data")
                      return(m)
                      ## Return a matrix that is the inverse of 'x'
                  }
                  data <- x$get()
                  m <- solve(data, ...)
                  x$setsolve(m)
                  m
                }
       
