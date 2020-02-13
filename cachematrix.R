## Overall this function is to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
   
    ## initialize two objects: x as an argument being a matrix and m being NULL
    inverse <- NULL
    
    ## assigns the input argument to x object in the parent enviroment, 
    ## m value being NULL in the parent enviroment.
    ## (it clears old value of inverse and recalculate when x is reset)
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## defines the getter for the vector x, which retrieves x value from parent enviroment.
    get <- function() x
    
    ## defines the setter for the inverse, which retrieves value from parent enviroment.
    setinverse <- function(solve) inverse <<- solve
    
    ## defines the getter for the inverse, which retrieves value from parent enviroment.
    getinverse <- function() inverse
    
    ## retur a list of named elements to the parent enviroment, to be used by downstream.
    list (set =set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## input will be the return from makeCacheMatrix function, which is a matrix of inverse "x"
cacheSolve <- function(x, ...) {
    
  ## assign value from getinverse function to inverse
    inverse <- x$getinverse()
    
    ## if inverse is not NULL, print message, 
    ## and retrieve cached inverse value return to parent enviroment.
    if(!is.null(inverse)) {
     message("getting cached data")  
      return(inverse)
      }
    
    ## if inverse value is NULL, run get() to get data
    data <- x$get()
    
    ## calculate data for inverse value
    inverse<- solve(data, ...)
    
    ## set inverse value to input object
    x$setinverse(inverse)
    
    ## return the inverse value to prent enviroment and print.
    inverse
}
