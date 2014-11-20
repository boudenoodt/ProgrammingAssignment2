## In order to avoid repeatedly (e.g. in a loop) time-consuming computations 
## in cases where the calculated value does not change, one creates a R function 
## that is able to cache this computation's result so that when the result is needed again , 
## it can be looked up in the cache rather than to be recomputed.
## The function "makeCacheMatrix"  constitues the cache and is used by the fuction 
## "cacheSolve" to create the cache.

## The end of this text gives a small example of the use of this functions.


## Comment on "makeCacheMatrix"

## The function, "makeCacheMatrix" creates a special "vector", which is really a list 
## containing  functions to :
## 		1.set the value of the matrix
## 		2.get the value of the matrix
## 		3.set the value of the inverse matrix
## 		4.get the value of the inverse matrix
## Remark that the "<<-" operator is used to assign a value to an object in an 
## environment (of another function) that is different from the current environment. 
## Important  here is to understand that you use "makeCacheMatrix" to create an object, 
## then access that constructed object, not "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
        t <- NULL
        set <- function(y) {
                x <<- y
                t <<- NULL
        }
        get <- function() x
        settransposed <- function(transposed) t <<- transposed
        gettransposed <- function() t
        list(set = set, get = get,
             settransposed = settransposed,
             gettransposed = gettransposed)
}

## Comment on "cacheSolve"

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed, then the cachesolve  retrieves the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        t <- x$gettransposed()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        data <- x$get()
        t <- solve(data)
        x$settransposed(t)
        t
}

####################
## example 
####################
## > m <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)	## create an invertible(check before) matrix
## > mcm <- makeCacheMatrix(m)						## create the function (object)
## > s<-cacheSolve(mcm)								## cacheSolve uses the makeCacheMatix (mcm parameter)
##													## to create the cached inverse matrix
## > s
##     [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## apply again and see that the inverse matrix comes from the cache
## s<-cacheSolve(mcm)
## getting cached data		## comes from the cache
## > s
##     [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4



## seting a new matrix
## v <-matrix(c(1,0,5,2,1,6,3,4,0), nrow = 3, ncol = 3) ## create a new matrix
## mcm$set(v)   									## using the setter method to store a new matrix
## > s<-cacheSolve(mcm)
## > s
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
# apply again and see that the inverse matrix comes from the cache
## s<-cacheSolve(mcm)
## getting cached data		## comes from the cache
## > s
#      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
