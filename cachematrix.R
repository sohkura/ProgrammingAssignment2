## Two functions are provided, makeCacheMatrix and cacheSolve functions. 
## makeCacheMatrix a special "matrix" object that can cache its inverse.
## ccheSolve computes the inverse of the special matrix returned by makeChacheMatrix.
## Usage: 
## mx is the original matrix
## my <- cacheSolve(makeCacheMatrix(mx))
## my is the inverse of mx where mx %*% my = Identify matrix


## The following function takes a matrix as an argument and 
## returns a list of four functions, set, get, setinv, and getinv.
## set(y) changes the matrix to y.
## get() returns the matrix previously set.
## setinv(inv) sets the inverse matrix inv.
## getinv() returns the inverse of the meatix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL    ## reset the inverse matrix
                              ## when the original matrix changes
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set,
                get = get, 
                setinv = setinv, 
                getinv = getinv)    
}


## The following function takes an object returned by makeCacheMatrix 
## and returns the inverse of the matrix.
## When the inverse has already been calculated, 
## it returns the inverse without calculation.
## Otherwise it calcualtes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
                ## case when the cache exists
                message("getting cached data")
                return(m)
        }
        ## case when the cache does not exsit 
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

