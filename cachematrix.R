## R-Programming Week3 Assignment

## makeCacheMatrix: return a list of functions to:
##    1. Set the value of the matrix
##    2. Get the value of the matrix
##    3. Set the value of the inverse
##    4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    ## the cached inverse will be stored in this variable
    inv <- NULL

    ## Getter & setter for the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    
    ## Getter & setter for the inverse
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Compute the inverse of the matrix. 
## If the inverse is already computed, it returns the cached value,
## otherwise it will compute an inverse and return.
cacheSolve <- function(x, ...) {
    ## Get the cached inverse and return it.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("retreive cached inverse")
        return(inv)
    }
    
    ## Calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## cache the computed inverse
    x$setinv(inv)
    
    ## return the inverse
    inv
}

## some tests

# mtrx = matrix(c(0,10,10,0),2,2)
# m <- makeCacheMatrix()
# m$set(mtrx)
# m$get()

## First run. An inverse will be calculated
# cacheSolve(m)

## Second run. An inverse will be retreived from the cache
# cacheSolve(m)


