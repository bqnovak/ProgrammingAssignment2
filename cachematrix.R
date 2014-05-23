## bqnovak

## We have 2 functions: makeCacheMatrix and cacheSolve.

## makeCacheMatrix sets the values for 2 the original matrix and the inverse matrix.
## cacheSolve calculates the inverse matrix of the matrix passed as parameter, 
## if the inverse matrix is already cached then it's recovered, 
## else the inverse matrix is calculated first.

## -------------------------------------------

## makeCacheMatrix function set matrix passed as data input, 
## this function defines 4 extra functions.
## setMatrix sets the value for x (original matrix) and inverseMatrix variables.
## getMatrix gets the original matrix passed as parameter to makeCache function.
## setInverseMatrix sets the value for inverseMatrix variable, 
## this value was calculated by cacheSolve function.
## getInverseMatrix gets the inverse matrix from inverseMatrix variable.

makeCacheMatrix <- function(x = matrix()) {

        inverseMatrix <- NULL

        setMatrix <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }

        getMatrix <- function() x

        setInverseMatrix <- function(iM) inverseMatrix <<- iM

        getInverseMatrix <- function() inverseMatrix 

        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## -------------------------------------------

## cacheSolve recovers inverseMatrix variable from makeCacheMatrix function.
## cacheSolve evaluates if inverseMatrix variable is null, 
## if it's not null then returns the value recovered before, 
## else inverse matrix is calculated and stored in inverseMatrix 
## variable of makeCacheMatrix via it's setInverseMatrix function.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        inverseMatrix<- x$getInverseMatrix()

        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }

        matrix <- x$getMatrix()

	  message("calculating inverse matrix for the very first time")
        inverseMatrix<- solve(matrix, ...)

        x$setInverse(inverseMatrix)
        inverseMatrix
}