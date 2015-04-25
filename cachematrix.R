## makeCacheMatrix and cacheSolve let you study the scoping rules in R

## i initialises an empty object
## set defines a function and introduces the superassignment operator: <<-
### set sets x to y and i to NULL in the *parent environment*
## get defines a function that will return x
## setinv defines a function that will calculate the inverse of a matrix and
### store the output as i in the parent environment (here = makeCacheMatrix env)
## getinv defines a function that will return i
## list will assign the four functions to the object of makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve returns a matrix that is the inverse of 'x'
## i is set to be the inverse of x
## the if-loop checks if there is already a value assigned to i and returns it
### in case there is one
## if i is NULL, data retrieves the original matrix
## i is then calculated as the inverse of x
## x$setinv(i) assigns the new i
## finally i is returned

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}