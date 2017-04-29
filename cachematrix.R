## Programming Assignment 2: Lexical Scoping
# These two functions are what is required for the assignment at the end of the third week
# of the Johns Hopkins Coursera R programming course.  
#
# I suggest using the following to generate the initial numerical square matrix required as input 
# to "makeCacheMatrix".
#       x <- replicate(10, rnorm(10))
#
# This takes a matrix 'x' and stores (1) it, a function (2) to recall 'x', (3) a function to 
# set the inverse of it and (4) a function to get the inverse of it, and returns all 4 in a list.
# It would all make more sense if we had studied object oriented programming.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        Set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        Get <- function() x
        SetInverse <- function(solve) inverse <<- solve  
        GetInverse <- function() inverse
        list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## Cache Solve(x) will return a matrix that is the inverse of 'x' IF x is the output from "makeCacheMatrix(x)
#
# This function takes the output from the "makeCacheMatrix" function, which should be 
# a list that contains a matrix and various functions from the "makeCacheMatrix" function.
# It checks to see if the inverse of that matrix has already been calculated and stored.
# If not, it calculates and returns the inverse, if it has already calculated the inverse
# it displays a message that it is retrieving the cached data, retrieves it and returns it.
# Otherwise it calculates and returns the inverse of the matrix/argument.
#
# Note:  The message is only displayed if "cacheSolve" is called indepently.  If something like
#        "my.inverse <- cacheSolve(makeCacheMatrix(x))" is used, then no message about "getting cached data"
#        is shown.
#
# Note: This does not test for validaty of the matrix (square and numeric and inversabile).  The function
#       "solve" does that, as the function is currently written.

cacheSolve <- function(x, ...) {
        inverse <- x$GetInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$Get()
        inverse <- solve(data)
        x$SetInverse(inverse)
        inverse
}
