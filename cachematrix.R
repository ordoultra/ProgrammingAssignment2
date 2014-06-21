## These functions create an object which will store a matrix and
## calculate it's inverse
## 
## Assumptions:  The matrix object is always square and invertable.
##

## The makeCacheMatrix function creates a list containing a function which 
## does the following:
##   - sets the value of the matrix
##   - gets the value of the matrix
##   - sets the value of the matrix's inverse
##   - gets the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x

    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## The cacheSolve function checks to see if a value has already been set for the
## matrix's inverse, if it has the set value is reused, if not it is calculated then set. 
cacheSolve <- function(x, ...) 
{
    i <- x$getinverse()
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }

    data <- x$get()
    i <- solve(x, ...)
    x$setinverse(i)

    return(i)
}
