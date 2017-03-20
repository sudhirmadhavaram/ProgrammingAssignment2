## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, cache's the inverse of matrix.

makeCacheMatrix <- function(mat = matrix()) {
        matinv <- NULL
        set <- function(z) {
                ## Assigning value to a object, that can be used in an environment different from this, using <<-.
                mat <<- z
                matinv <<- NULL
        }
        ## get the matrix
        get = function() mat
        ## set the inverse 
        setinv <- function(inverse) matinv <<- inverse 
        ## get the inverse
        getinv <- function() matinv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(mat) {
        matinv <- mat$getinv()
        
        ## Checking, it the matinv object has cached the inverse of matrix defined in makeCacheMatrix
        if (!is.null(matinv)){
                message("printing the inverse matrix from cache")
                return(matinv)
        }
        
        ## Calculate the inverse of matrix, if the cache is null
        matrix_data.data <- mat$get()
        matinv <- solve(matrix_data.data)
        
        #Sets the inverse of matrix in cache, using setinv
        mat$setinv(matinv)
        
        return(matinv)
}

## Sample calculation, using a 10 by 10 random normal distributed matrix
mat = matrix(rnorm(100), 10, 10)
minverse = makeCacheMatrix(mat)
cacheSolve(minverse)

