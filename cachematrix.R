## These functions create an object which will store a matrix and
## calculate it's inverse.
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
	#attempt to get previously set inverse of matrix x
    i <- x$getinverse()

	#if inverse is not null return the inverse matrix
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }

	#set matrix x to data
    data <- x$get()
	#invert data and set to inverse matrix to i
    i <- solve(data, ...)
	#use x's setter function to set inverse matrix
    x$setinverse(i)

    return(i)
}

#> mat1 <- matrix(c(1,2,3,0,1,4,5,6,0), 3)
#> mat1
#     [,1] [,2] [,3]
#[1,]    1    0    5
#[2,]    2    1    6
#[3,]    3    4    0
#> x <- makeCacheMatrix(mat1)
#> x$get()
#     [,1] [,2] [,3]
#[1,]    1    0    5
#[2,]    2    1    6
#[3,]    3    4    0
#> cacheSolve(x)
#     [,1] [,2] [,3]
#[1,]  -24   20   -5
#[2,]   18  -15    4
#[3,]    5   -4    1
#> x$getinverse()
#     [,1] [,2] [,3]
#[1,]  -24   20   -5
#[2,]   18  -15    4
#[3,]    5   -4    1

