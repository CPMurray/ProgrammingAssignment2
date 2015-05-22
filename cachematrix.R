## The two functions in this file implement a caching system to store the 
## inverse of a matrix so that if it needs to be calculated multiple times,
## clock cycles will not have to be wasted repeating calculations.

## These functions have been tested successfully with the following:
##                [1 2 3]                 [-24  18  5]
## input matrix = [0 1 4]  ; its inverse  [ 20 -15 -4]
##                [5 6 0]                 [  5   4  1]

## The function makeCacheMatrix creates a list of functions that allow the 
## inverse solution to a matrix to be cached.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL # initialize m for inverse pre-existence check
      
      # from  help ("<<-"): "The operators <<- ... cause a search to (be) made 
      #     through parent environments for an existing definition of the 
      #     variable being assigned. If such a variable is found ... then its 
      #     value is redefined, otherwise assignment takes place in the 
      #     global environment.
      
      set <- function(y) {
            x <<- y     # make x and m available to cacheSolve by instantiating 
            m <<- NULL  #    in parent environment
      } # end function
      
      get <- function() {  # returns the input matrix
            x
      } # end function
      
      setinverse <- function(solve) {  # assigns the inverse solution to m in 
            m <<- solve                #    the parent environment
      } # end function
      
      getinverse <- function() {  # retrieve the value of inverse in m
            m
      } # end function
      
      # this function return a list of functions
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
} # end function


## The cacheSolve function utilizes functions in makeCacheMatrix to either 
## calculate the inverse of a matrix if it is not already stored in cache, or 
## if it is, to retrieve it and return it to calling environment.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinverse() # check to see if there is already a value for inverse
                          #   recall that m was initialized with null in 
                          #   function makeCacheMatrix; if inverse has already
                          #   been calculated, m will not be null because it 
                          #   will be assigned the value that is in cache
      
      if(!is.null(m)) { # if not null, execute the contents of if block
            message("getting cached data")
            return(m) # the function call returns (cached) m
      }
      # if not returning cached data, the following will be executed
      data <- x$get()   # get the input matrix and assign to data
      m <- solve(data, ...) # send matrix to inverse solver function
      x$setinverse(m)   # set the value of inverse so we can check to see if the 
                        #     inverse is in cache if called to find the inverse
                        #     of the input matrix again
      m # the function call returns (newly calculated, non-cached) m
      
} # end function