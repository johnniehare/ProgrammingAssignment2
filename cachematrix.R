## The function "makeCacheMatrix" creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x) { ## x needs to be a matrix object which is
                                 ## using the solve() function. There is no
                                 ## error handling done in this function!
  m <- NULL
  set <- function(y) {           ## initializing the function and storing the 
    x <<- y                      ## storing the object
    m <<- NULL                   ## cleaning up storage variable
  }
  get <- function() x            ## initializing the store function.
  setinverse <- function(solve) m <<- solve  ## handing over solve() as method
  getinverse <- function() m     ## giving back the value
  list(set = set, get = get,     ## creating the whole "function list" as a separate
       setinverse = setinverse,  ## environment
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Returns a matrix that is the inverse of 'x'
  m <- x$getinverse()            ## get back the inverted matrix
  if(!is.null(m)) {              ## if m is empty then the inverted matrix is still the same
    message("getting cached data")   ## show that this is cached data
    return(m)                    ## deliver m
  }
  data <- x$get()                ## if data has changed,
  m <- solve(data, ...)          ## compute inversion
  x$setinverse(m)                ## store inversion in variable m
  m                              ## deliver m
}
