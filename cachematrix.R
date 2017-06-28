## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Importing `MASS` package for inversing matrix using ginv() function to
## avoid `Lapack routine dgesv: system is exactly singular: U[3,3] = 0` error
## generated from using solve() function   

require(MASS)

makeCacheMatrix <- function(x = matrix()) {
    
    ## Checking if the user truly passed a valid Square Matrix or not before
    ## running the function
    
    if(is.matrix(x) && nrow(x) == ncol(x)){
        
        inv <- NULL
        
        set <- function(y) {
            
            x <<- y
            
            inv  <<- NULL
            
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv  <<- inverse
        
        getInverse <- function() inv 
        
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

     ## if the user not passing a valid Square Matrix retun the following
     ## error message
        
    } else {
        
        message("Error, Please re-enter a valid Square Matrix")
        
        
    }
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    
    ## Double check the following :
    ## 1. the inversed matrix is already set or is null
    ## 2. the passed or Global Matrix (gblMatrix) is the same as the Stored Matrix 
    ##    or not (x$get())
    ## if the inversed matrix is set and the Matrix doesn't changed, so return the inversed matrix from
    ## the cache, and if not, calculate the inverse for the provided New Matrix
    
    if(!is.null(inv) && identical(x$get(), gblMatrix)) {
        
    ## Return a message for indicating that the returned inversed matrix was from `cached data`
        
        message("getting cached data")
        
        return(inv)
        
    }
    
    data <- x$get()
    
    inv <- ginv(data, ...)
    
    x$setInverse(inv)
    
    ## Return a message for indicating that the returned inversed matrix was from `New Calculation`
    
    message("calculating data")
    
    inv
}

## Running the functions as follow
## 1. Set the Global Matrix the will passed to the `makeCacheMatrix` function 

gblMatrix <- matrix(1:16, 4,4)

## 2. Store the returned Matrix from `makeCacheMatrix` function to re-pass it
##    again to the `cacheSolve` function

rtnMatrix <- makeCacheMatrix(gblMatrix)

## 3. Passing the returned matrix from `makeCacheMatrix` function to
##    the `cacheSolve` function for inversing

cacheSolve(rtnMatrix)