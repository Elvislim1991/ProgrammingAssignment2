## Below is 2 functions that can cache the result for matrix
## inversion (Provided it is invertible)

## This 1st function take matrix as input and create a list of function to
## 1) set the value of matrix
## 2) get the value of matrix
## 3) set the value of matrix inversion
## 4) get the value of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
        inv.m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv.m <- function(solution) inv.m <<- solution
        getInv.m <- function() inv.m
        list(set = set, get = get,
             setInv.m = setInv.m,
             getInv.m = getInv.m)
}


## This function will then take the result of the list from makeCacheMatrix 
## to get the inverse of the matrix.
## It will first check whether the matrix inversion had been set. If so, it 
## will get the result from the cache.
## Otherwise, it will calculates the matrix inversion and set the result to the
## cache through setInv.m function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.m <- x$getInv.m()
        if(!is.null(inv.m)) {
                message("getting cached data")
                return(inv.m)
        }
        data <- x$get()
        inv.m <- solve(data, ...)
        x$setInv.m(inv.m)
        inv.m
}
