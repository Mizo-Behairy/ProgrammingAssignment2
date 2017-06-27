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
        
        m <- NULL
        
        set <- function(y) {
            
            x <<- y
            
            m <<- NULL
            
        }
        
        get <- function() x
        
        setmean <- function(mean) m <<- mean
        
        getmean <- function() m
        
        list(set = set, 
             get = get,
             setmean = setmean,
             getmean = getmean)

     ## if the user not passing a valid Square Matrix retun the following
     ## error message
        
    } else {
        
        message("Error, Please re-enter a valid Square Matrix")
        
        
    }
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmean()
    
    ## Double check the following :
    ## 1. the mean is already set or is null
    ## 2. the passed or Global Matrix (gblMatrix) is the same as the Stored Matrix 
    ##    or not (x$get())
    ## if the mean is set and the Matrix doesn't changed, so return the mean from
    ## the cache, and if not, calculate the mean for the provided New Matrix
    
    if(!is.null(m) && identical(x$get(), gblMatrix)) {
        
    ## Return a message for indicating that the returned mean was from `cached data`
        
        message("getting cached data")
        
        return(m)
        
    }
    
    data <- ginv(x$get())
    
    m <- mean(data, ...)
    
    x$setmean(m)
    
    ## Return a message for indicating that the returned mean was from `New Calculation`
    
    message("calculating data")
    
    m
}

## Running the functions as follow
## 1. Set the Global Matrix the will passed to the `makeCacheMatrix` function 

gblMatrix <- matrix(1:16, 4,4)

## 2. Store the returned Matrix from `makeCacheMatrix` function to re-pass it
##    again to the `cacheSolve` function

rtnMatrix <- makeCacheMatrix(gblMatrix)

## 3. Passing the returned matrix from `makeCacheMatrix` function to
##    the `cacheSolve` function for inversing then calculating mean

cacheSolve(rtnMatrix)

