# makeCacheMatrix 

# makeCacheMatric builds a set of four functions, set(), get(), setinv(), getinv()
# Descriptions for each are inline with the function
# These store and retrieve a matrix and inverse matrix in conjunture with cacheSolve.R
# They are stored as a list to be returned to the parent environment
#
# After reading the tutorial it was clear the most efficient solution was to just modify the assignment code. 

makeCacheMatrix <- function(x = matrix()) {   
  i <- NULL                        # obj to store inverse matrix
                                   # this will be available for all makeCacheMatrix() functions 
  set <- function(y) {
    x <<- y                        # assign input argument into x in the parent environment because of <<- operator
    i <<- NULL                     # reset the matrix i, if set() is called we should overwrite what cacheSolve has already set in i
  }                               
  get <- function() {
    x                              # x is retrieved from parent env, not local to get()
  }
  
  setinv <- function(inv) {
    i <<- inv                      # no value until called from cacheSolve
  }                                # <<- is used so i is updated in the parent env. and can be used in other functions.
  
  getinv <- function() {
    i                              # return what is in i if called
  }
    
  list(set = set, get = get,       # store the 4 makeCacheMatrix functions in a list
       setinv = setinv,            # so when the function is stored in an obj. they can be retrieved with the $ form 
       getinv = getinv)            # of the extract operator
}



# cacheSolve

# cacheSolve checks if the inverted matrix for a given matrix has already been calculated.
# If so it will return the cached inverted matrix, skipping re-calculating
# If not it will calculate the inverted matrix and return that matrix.

cacheSolve <- function(x, ...) {  
  i <- x$getinv()                    # attempt to retrieve an inverted matrix of the current values
  if(!is.null(i)) {                  # check to see if any value was returned
    message("getting cached data")   # if a value exists, print the message
    return(i)                        # send back the inverted matrix that already exists
  }
  data <- x$get()                    # retrieve current matrix and store in temp variables
  i <- solve(data, ...)              # calculate the inverse of the matrix in temp data variable
  x$setinv(i)                        # return inverted matrix to makeCacheMatrix
}




# EXECUTION EXAMPLE
#
# > m1
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
#
#
# > a <- makeCacheMatrix(m1)
#
#
# > a$get()
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
#
#
# > cacheSolve(a)
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > 
# 
#
#  > cacheSolve(a)
# getting cached data
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > 
