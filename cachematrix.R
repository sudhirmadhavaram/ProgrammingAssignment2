## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
        matinv <- NULL
        set <- function(z) {
                matinv <<- z
                matinv <<- NULL
        }
        get = function() mat
        setinv <- function(inverse) matinv <<- inverse 
        getinv <- function() matinv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(mat) {
        matinv <- mat$getinv()
        
        if (!is.null(matinv)){
                message("printing the inverse matrix from cache")
                return(matinv)
        }
         
        matrix_data.data <- mat$get()
        matinv <- solve(matrix_data.data, ...)
        
        mat$setinv(matinv)
        
        return(matinv)
}


mat = matrix(rnorm(100), 10, 10)
minverse = makeCacheMatrix(mat)
cacheSolve(minverse)

