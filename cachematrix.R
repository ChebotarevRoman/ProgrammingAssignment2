## Two functions which can be used in combination to store matrix with its inverse
## Inverse matrix is cached. So if matrix not change, inverse will not be recomputed
## Instead cached inverse matrix will be returned.
##
## Detailed description of this two functions can be found below.

## ------------------------------------------------------------------

## This function creates CacheMatrix which is really a list of special functions:
## 1. set matrix
## 2. get matrix
## 3. set inverse matrix
## 4. get inverse matrix
##
## So CacheMatrix allows to store (cache) inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse matix
    inv.m <- NULL
    
    # set matrix. If setting new matrix, old inverse matrix value clearing
    set <- function(y) {
        x <<- y
        inv.m <<- NULL
    }
    
    # get matrix
    get <- function() x
    
    # set inverse matrix
    set.inv <- function(m) inv.m <<- m
    
    # get inverse matrix
    get.inv <- function() inv.m
    
    # return CacheMatrix which is a list of functions
    list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}

## ------------------------------------------------------------------

## This functions calculates inverse matrix of CacheMatrix.
## If inverse matrix has already presented (cached), then return it

cacheSolve <- function(x, ...) {
    
    # geting inverse matrix value
    inv.m <- x$get.inv()
    
    # if it exists (cached), then return it
    if(!is.null(inv.m)) {
        message("returning cached data")
        return(inv.m)
    }
    
    # else get matrix itself, ...
    m <- x$get()
    # ... calculate its inverse, ...
    inv.m <- solve(m)
    # ... cache in CacheMatix ...
    x$set.inv(inv.m)
    # ... and return
    inv.m
}
