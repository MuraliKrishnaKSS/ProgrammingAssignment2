## makeCacheMatrix function receives a matrix and creates a list of four functions described below. 
## 1.  "get" function returns the matrix that is passed on to makeCacheMatrix as an attribute
## 2.  "set" function initializes value of variable 'm' and sets value of 'x' to the value of attribute "y' received through the funciton
## 3.  "setinverse" function assigns value received through attibute to variable "m"
## 4.  "getinverse" function returns value of variable "m"
##------------------------------------

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## CacheSolve function receives makeCacheMatrix along with a matrix as latter's attribute 
##              and returns an inverse matrix. The function checks the cache, and returns 
##              inverse matrix that has been previously computed and stored in the cache.
##              Function will display a message to indicate that data is furnished from cache.
##              In case cache does not have previously stored inverse matrix, then the 
##              function calculates inverse matrix afresh. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
