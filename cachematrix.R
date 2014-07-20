## These two functions are used to create a special object that stores a matrix and cache's its inverse.

## This function, makeCacheMatrix creates a list containing 4 functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set and get functions
    set <- function(y) {         
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    ## Define list with the set and get functions.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## The following function calculates the inverse of the matrix set in the list object created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets it's value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {                ## Check to see if inverse already exists  
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()                  ## If inverse doesn't already exist, get it, calc its inverse, store it in cache.
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
