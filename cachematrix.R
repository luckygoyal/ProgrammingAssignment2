## Put comments here that give an overall description of what your
##name: Mayank Goyal
## This function will compute the inverse matrix using cache
## This function will create an image in a different environment using the <<- operator 


makeCacheMatrix <- function(x = matrix()) {

       z <- NULL
       Set <- function(y) {
            x <<- y
            z <<- NULL
        }
        Get <- function() x
        SetInverse <- function(mean) z <<- mean
        GetInverse <- function() z
        list(Set = Set, SetInverse = SetInverse, Get = Get, GetInverse = GetInverse)
}


## This function computes the inverse of a matrix using solve function

cacheSolve <- function(x, ...) {
       z <- x$GetInverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$Get()
        z <- solve(data, ...)
        x$SetInverse(z)
        z
        ## Return a matrix that is the inverse of 'x'
}
