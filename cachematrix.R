## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL #set variable iM (where inverted matrix will be stored) to NULL
  set <- function(y){ #this function can directly set matrix to passed value
    x <<- y #set matrix to passed value
    iM <<- NULL #resets 
  }
  get <- function() x #function returns matrix
  setInv <- function(invMat) iM <<- invMat #function assigns placeholder iM to actual inverse matrix
  getInv <- function() iM #function returns iM, may or may not be inverse matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) #this is the list of functions, can be passed
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) { #passes the list of functions
  iM <- x$getInv() #calculates inverse matrix
  if(!is.null(iM)) { #checks to see if iM already assigned to inverse matrix
    message("getting cached data") 
    return(iM) #returns already assigned inverse matrix
  }
  data <- x$get() #grabs original matrix
  iM <- solve(data, ...) #calculates and assigns inverse matrix to iM
  x$setInv(iM) #also sets iM to inverse matrix in other function (cached value)
  iM
}
