## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix object as argument and defines
## an object that implements caching fuctionality.
## Arguments: x, a matrix object
## Usage: call with a matrix as argument, assign a result to a
## variable to use later.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize s variable - it stores the solution
        ## (inverse in that case).
        s <- NULL
        
        ## A bunch of functions that
        ##      a) set a value of a stored matrix and re-init s;
        ##      b) get a value of a stored matrix;
        ##      c) stores a solution in the s variable;
        ##      d) gets a solution from s variable.
        ## The s variable "lives" in an environment that is parent
        ## for these functions, so any of them can access it.
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsol <- function(sol) s <<- sol
        getsol <- function() s
        
        list(set = set, get = get,
             setsol = setsol, getsol = getsol)
}


## This function inverse the matrix using the caching
## fuctionality.
## Arguments: x - the makeCacheMatrix object.
## Usage: initialize an object with makeCacheMatrix function,
## store it in a variable, e.g. my.x, then call cacheSolve with
## my.x as a parameter.

cacheSolve <- function(x, ...) {
        
        s <- x$getsol()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        
        data <- x$get()
        s <- solve(data)
        x$setsol(s)
        s
}
