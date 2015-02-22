## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #this line establishes the "makeCacheMatrix" function, where a matrix "x" is the argument
        #body of funciton is below
        i <- NULL ## creates object i that will store the inverse of matrix x, it is set to NULL
        set <- function(y) { #creates function "set" with argument y 
                x <<- y 
                i <<- NULL
        } 
        get <- function() x #sets "get" to x
        setinv <- function(solve) i <<- solve ##cache inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #the output of "makeCacheMatrix" creates a list of the functions defined above: set, git, setinv,  getinv
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'       
        i <- x$getinv()  #sets "i" to the value of "getinv" stored in x, the "makeCacheMatrix"
        if(!is.null(i)) { #First "cacheSolve" checks to see if the inverse, "i", has already been calculated
                message("getting cached data")
                return(i) ##if i is null, returns "NA"
        }
        data <- x$get() #sets object "data" to the value of "get" stored in x, the "makeCacheMatrix"
                        #this will be the value of "x" as specified in the "makeCacheMatrix" function
        i <- solve(data, ...) #calculates the inverse of "data," which should be the inverse of "x" specified in the "makeCacheMatrix"
        x$setinv(i) #sets the value of "setinv" in x to the value of the inverse defined as "i"
        i #return i
}
