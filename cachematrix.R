## Put comments here that give an overall description of what your
## functions do

## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #this line establishes the "makeCacheMatrix" function, where a matrix "x" is the argument
        #body of funciton is below
        i <- NULL ## creates object i that will store the inverse of matrix x, it is set to NULL
        set <- function(y) { #creates function "set" with argument y where x from above is set to y
                x <<- y
                i <<- NULL
        } ## the set function sets 
        get <- function() x
        setinv <- function(solve) i <<- solve ##cache inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'       
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i) ##returns "NA" because i is null.
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i 
}
