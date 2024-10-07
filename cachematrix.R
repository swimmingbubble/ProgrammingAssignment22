## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCachematrix<-function(m=matrix()){                      #function that takes a matrix as an argument and stores its inverse.
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


## Write a short comment describing this function

cacheSolve <- function(m, ...) {                                        #function that takes the previous function and calculates the inverse and stores it, if the inverse hasn't been calculated yet. 
        inv <- m$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinv(inv)
        inv
}
