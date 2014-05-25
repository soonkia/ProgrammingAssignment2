## Put comments here that give an overall description of what your
## functions do

## Create an initial Matrix Cache that will allow us to set a matrix
## We then can use InvMatrix function to create and return the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## m is the variable for the that will hold the inverted matrix
  ## x is the variable that will hold the original matrix
  m <- NULL
  ## used to set a the matrix - we clear m as we will be loading a new
  ## matrix
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  ## returns the current, univerted matrix
  get <- function() x
  ## returns the inverted matrix. Returns NULL if setInvMatrix hasn't been run
  getInvMatrix <- function () m
  ## inverts the matrix using solve and places it into variable m
  setInvMatrix <- function(data) m<<-solve(data)
  ## the availabe function that are available to makeCacheMatrix
  list(set=set,get=get,getInvMatrix=getInvMatrix,setInvMatrix=setInvMatrix)
}


## We pass a cachematrix matrix to cacheSolve, which then allows will then 
## check if we have cached the Inverse of the matrix yet. If we haven't it
## will compute the Inverse for us, and then return the result, else it will
## return the previously computed and stored(cached) matrix for us

cacheSolve <- function(x, ...) {
    ##First we check if invMatrix has been set
    m <- x$getInvMatrix()
    if (!is.null(m)) {
      #If inverse matrix has been set return it
      message("Returning cached data")
      return(m)
    }
    ##Inverse Matrix hasn't been set so we need to calculate it
    ##First we get the matrix we are working with
    data <- x$get()
    ##Now we send it to be solved and set
    x$setInvMatrix(data)
    ##Now we return the inverse matrix - next time we run it, 
    ##it will use the cached version
    x$getInvMatrix()
}
