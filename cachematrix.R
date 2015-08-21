## cachematrix.R done by Fernando Flores for coursera R Programming course
## (August 2015 - Programming assignment 2)

## This source implements two functions in order to provide ways to invert
## a matrix once and keep it in cache. This requirement is useful when the
## matrix is large and the solve process will take big amount of time.
##
## For test purposes, consider always an invertible matrix.
## The way to test if a squared matrix is invertible in R is using the det() 
## function which returns a numeric vector with one element. If det(somematrix) 
## returns the value 1, means the matrix "somematrix" is invertible. You can
## use ?det and take a look at the function documentation.
##
## Usage example:
##
## A 3x3 matrix that can be inverted is:
## M = (1 2 3)
##     (0 1 4)
##     (5 6 0)
##
## The inverse matrix of M is:
##
## M^-1 = (-24  18   5)
##        ( 20 -15  -4)
##        ( -5   4   1)
##
## Using the R command line:
##
## myMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
## det(myMatrix)
## myCacheMatrix <- makeCacheMatrix(myMatrix)
## cacheSolve(myCacheMatrix)
## 
## For more references about the subject of invertible matrix, see:
## https://en.wikipedia.org/wiki/Invertible_matrix


## makeCacheMatrix() returns a list with 4 functions (2 setters and 2 getters)
## which allows to keep in memory the inverse matrix of the original one.
##
## Parameters:
## x = The original matrix to process and try to invert

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    setMatrix <- function(matrix) {
        x <<- matrix
        inverseMatrix <<- NULL
    }
    
    getMatrix <- function() {
        x
    }
    
    setInverseMatrix <- function(inverse) {
        inverseMatrix <<- inverse
    }
    
    getInverseMatrix <- function() {
        inverseMatrix
    }
    
    # Return a list with 2 getters and 2 setters (for the matrix and the 
    # inverse matrix)
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve() implements the conditions to use the cached version of the
## inverSe matrix if possible. The first time is executed for a matrix,
## the matrix will be inverted and kept in memory through the function
## makeCacheMatrix().
##
## Parameters:
## x = The "special matrix" result obtained after executing makeCacheMatrix()
##     over some matrix
## ... = Other parameters than can be used on the solve() function if needed

cacheSolve <- function(x, ...) {
    ## If the matrix is not cached, returns the inverse matrix from the cache
    inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("Getting the cached inverse matrix")
        return(inverseMatrix)
    }
    
    ## Invert the matrix
    data <- x$getMatrix()
    inverseMatrix <- solve(data, ...)
    
    ## Save the inverse matrix on the cache
    x$setInverseMatrix(inverseMatrix)
    
    ## Returns the inverse matrix after computing it the first time
    inverseMatrix
}
