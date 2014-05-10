## The following function works for caching the inverse of a matrix  in matrix 
## inversion procedure

## Function makeCacheMatrix does the following....
## set the value of the matrix which is specified in the procedure 
## get the value of the matrix that has been set 
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x <<- y
                inv <<- inv
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list( set=set, get=get,
        setsolve = setsolve
        getsolve = getsolve)
}


## Function cacheSolve calculates the inverse of the matrix created above
## in the following manner......
## see if the inverse has been already calculated
## if so, get the inverse from the cache and skip calculation
## or else, calculate the inverse of the data and set it in the cache via setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve (data,...)
        x$setsolve(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
