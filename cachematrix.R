# Programming assignment 2 in course "R Programming". The assignment containds a pair of functions
# that cache the inverse of a matrix.
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix(x) creates a customized matrix object that is able to store its cached
## inverted value
makeCacheMatrix <- function(x = matrix()) 
{
    invertedMatrix <- NULL ### cached inverted value of the matrix
    set <- function(value) ### copy constructor, by changing the source object cache is cleared
    {
        x <<- value
        invertedMatrix <<- NULL
    }
    get <- function() x ### return created matrix
    setInvertedValue <- function(inverted) invertedMatrix <<- inverted ### cache the inverted value
    getInvertedValue <- function() invertedMatrix
    list(set = set, get = get, setInvertedValue = setInvertedValue, 
         getInvertedValue = getInvertedValue)
}


## cacheSolve() calculates inverted value of the given matrix and provides caching of the
## calculated value. The given matrix should always be invertible, otherwise additional
## verification is required
cacheSolve <- function(x, ...) 
{
    invertedMatrix <- x$getInvertedValue()
    if(!is.null(invertedMatrix)) ### if the value was already cached then do not calculate it again
    {
        message("The inverted matrix was already cached, retrieving its value...")     
    }
    else
    {
        data <- x$get() ### retrieve matrix data
        invertedMatrix <- solve(data) ### invert it
        x$setInvertedValue(invertedMatrix) ### store cache
    }
    return(invertedMatrix) ### Return a matrix that is the inverse of 'x'
}
