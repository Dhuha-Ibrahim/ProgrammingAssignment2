# this function will return a list of 4 functions (set,get,setIv and getIv)

makeCacheMatrix <- function(x = matrix()) 
{
  
  xinv <- NULL 
  set <- function(y) 
  {
    x <<- y
    xinv <<- NULL # assign the inverse of the matrix to Null if the inverse has been calculated before
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  
  list(set = set, get = get,setInv = setInv,getInv = getInv) 
}


cacheSolve <- function(x, ...) 
{
  m <- x$getInv() # get the inversed matrix from object x
  
  if(!is.null(m)) # check if the value of the matrix is null, which mean the inverse of the matrix has been calculate
  { 
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  
  data <- x$get() # if not, get the matrix object
  m <- solve(data) # use solve function to get the inverse of the matrix
  x$setInv(m) #  set it to the object
  m # return the solved result
  
}