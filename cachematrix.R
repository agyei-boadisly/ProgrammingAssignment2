## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                ## set initial value of inv
                inv <- NULL
                
                set <- function(y) {
                        
                        ## assign the value of y to x (stored in the parent environment)
                        x <<- y
                        
                        ## assign NULL in inv (stored in the parent environment)
                        inv <<- NULL
                }
                ## return the matrix x
                get <- function() x
                
                ## sets the value of the inverse matrix to the inv variable (stored in the parent environment)
                setinverse <- function(inverse) inv <<- inverse
                
                ## return the cached value of inverse matrix x
                getinverse <- function() inv
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## function to compute the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
                
                ## assign the value of the cached inverse matrix from cacheMatrix to inv variable
                inv <- x$getinverse()
                
                ## if the value is different than null(meaning it was computed already),return the value of inversed matrix A
                if(!is.null(inv)) {
                        message("Getting Cached Data")
                        return(inv)
                }
                
                ## assign the value of x to a matrix named mymatrix 
                mymatrix <- x$get()
                
                ## store the value of inversed matrix x to inv variable
                inv <- solve(mymatrix)
                
                ## cache the inv variable for further usage and printing inv
                x$setinverse(inv)
               
               ## Return a matrix that is the inverse of 'x'
                inv
}
