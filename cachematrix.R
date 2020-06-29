## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

# The function makeCacheMatrix creates a special "matrix"
# at the end what you get is a list that contains a function for:
# set the value of the matrix
# get the value of the matrix
# set the value of the solve matrix
# get the value of the solve matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function

#The following function calculates the inverse of the special
#matrix created with the previous function. However, it is first
#verified if the inverse has not been calculated, if so, it gets 
#the inverse of the cache and omits the calculation. Otherwise, 
#calculates the inverse of the matrix and sets the value of the 
#inverse in the cache using the function setInverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat,...)
    x$setInverse(i)
    i
}
