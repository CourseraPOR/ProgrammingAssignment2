## These functions (1) create a series of get and set functions to set/get cached values for a matrix and its inverse, 
## and (2) check whether a value has been set for the inverse of the matrix 
## if inverse matrix has been calculated, it will be returned
## if inverse has not been calculated, it will be calculated using solve() function


## create a series of get and set functions to set/get cached values for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL          ##create an empty matrix that will hold the inverse matrix
  setMatrix <- function(y) {     ##create set function that will define the matrix to be inverted, accepts a single argument (the matrix)
    x <<- y                      ##set x (matrix) based on the argument passed to setMatrix
    inverseMatrix <<- NULL       ##(re)set the inverseMatrix to NULL (because the matrix has changed)
  }
  
  getMatrix <- function() x      ##create get function, accepts no argument, returns x (the matrix)
  
  setInverseMatrix <- function(inv) inverseMatrix <<- inv     ##create set function accepts one argument and uses this to set value for inverseMatrix
  
  getInverseMatrix <- function() inverseMatrix                ##create get function, accepts no argument, returns inverseMatrix
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)  ##create a list to hold the functions created

}


## check whether a value has been set for the inverse of the matrix 
## if inverse matrix has been calculated, it will be returned
## if inverse has not been calculated, it will be calculated using solve() function

cacheSolve <- function(x, ...) {     ##cacheSolve accepts one (or more) arguments, x = makeCacheMatrix function
  inverse <- x$getInverseMatrix()    ##call the getInverseMatrix function to see if a value has been set for inverseMatrix, use value to set inverse
  if(!is.null(inverse)) {            ##if a value has now been set for inverse
    message("getting cached inverse matrix")
    return(inverse)                  ##return the inverse matrix
  }
  else {                             ##if inverse is still null after the getInversematrix call...
    matrixForInversion <- x$getMatrix()    ##create a matrix that is to be inverted by calling getMatrix() from x
    inverse <- solve(matrixForInversion)   ##create the inverse of the matrix by calling solve()
    x$setInverseMatrix(inverse)            ##use the inverse matrix just calculated to set the value for inverseMatrix
  }
        
  inverse ## Return a matrix that is the inverse of 'x'
}
