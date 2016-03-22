## cachematrix.R
#These 2 functions are useful to cache inverse of a given matrix
#to save computation time in future.

## First of these 2 functions names makeCacheMatrix is required to set an object
#containing given matrix as well as its cached inverse. It has following functions:
#1) set - to set (and reset) matrix in this object; it also makes inverse
#of the previous matrix equal to NULL after resetting
#2) get - returns matrix that is saved in this object
#3) setinv - to set inverse of the matrix; it doesn't calculate the inverse,
#it only sets any object as an inverse
#4) getinv - returns inverse of the matrix that is saved in the object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(compinv) inv <<- compinv
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


#Second of these functions, cacheSolve, is useful to obtain inverse of the matrix
#from the object created by makeCacheMatrix function if it is found in its cache.
#If it doesn't exist there, cacheSolve function calculates the inverse using
#Solve function and caches obtained matrix in the corresponding object of
#makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        invmatrix <- x$getinv()
        if (!is.null(invmatrix)){
                message("Getting cashed data")
                return(invmatrix)
        }
        else{
                message("Calculating inverse of matrix")
                invmatrix <- solve(x$get())
                x$setinv(invmatrix)
                return(invmatrix)
        }
}
