## This function will get, set,  set inverse or get inverse based on globally set variables x1 and inv
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
    x1 <<- y      # Cache the matrix
    inv <<- NULL #Inverse variable in different environment
    }
    get <- function() x1 # Return cached matrix
    
    setInverse <- function(inverse=matrix()) inv <<- inverse
    getInverse <- function() inv   ##Return inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  

}


## This function compares if the function passed is the same as the one stored in global variable, 
## if different, it will calculate inverse, else return inverse calculated by the last call of this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    matrix<- makeCacheMatrix()


## Comparing the new matrix and the one passed last time
    if (exists("inv") && is.matrix(x) && is.matrix(matrix$get()) && dim(x) == dim(matrix$get()) && all(x == matrix$get())){
          message("getting cached data");
          return(matrix$getInverse());
    }
    
    # Else
    print ("Not using cache data")
    matrix$set(x)  # Store new matrix
    data <- matrix$get()
    matrixInverse <- solve(data, ...)
    matrix$set(x)
    print(matrix$get())
    matrix$setInverse(matrixInverse)
    # Return Matrix inverse
    matrixInverse
}
