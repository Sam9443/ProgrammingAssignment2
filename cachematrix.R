## The first function, makeCacheMatrix creates a matrix, which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        #sets the matrixInv variable to NULL
        matrixInv <- NULL
        # function to set values accordingly
        setMatrix <- function(set_mat) {
                x <<- set_mat
                #sets the matrixInv variable to NULL
                matrixInv <<- NULL
        }
        #returns the matrix
        getMatrix <- function() x
        # set the inverse of the matrix
        setInv <- function(Inv) matrixInv <<- Inv
        # get the inverse of the matrix
        getInv <- function() matrixInv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInv = setInv, getInv = getInv)
}

## cache solve is a function to find whether there exists a cached copy
## if the inverse matrix
## if there is no cached copy then it calculates the inverse of matrix and the cache it.

cacheSolve <- function(x, ...) {
        
        m <- x$getInv()
        # checks whether there is a cached copy of inversed matrix
        m
        if(!is.null(m)) {
                message("getting cached data")
                # if yes then returns the cached copy
                return(m)
        }
        # as a cached copy is not there, it gets the original matrix by using function
        # getMatrix()
        data <- x$getMatrix()
        data
        # here it calculates the matrix inverse
        m <- solve(data)
        # it caches the calculated inverse for further use by using function set
        x$setInv(m)
        m
}
## Output
#  
# source("cachematrix.R")
# x <- matrix(c(4,3,3,2),2,2)
# > x
#       [,1]  [,2]
# [1,]    4    3
# [2,]    3    2
# > m <- makeCacheMatrix(x)
# > m$getMatrix()
#       [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# m$getInv()
# NULL
# cacheSolve(m)
#       [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# calling cacheSolve() again
# > cacheSolve(m)
# getting cached data
#       [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# checking whether the cached variable has the inverse of the matrix
# m$getInv()
#       [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4