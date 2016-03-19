## The following functions are used to cache the inverse of a matrix

## Calculating the inverse of a matrix is memory intensive and it makes sense
## to cache such calculations in a variable which can be reused across the scope

## The following function 'makeCacheMatrix' achieves the following
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        aMatrix <- NULL
        
        ## set the value of the matrix
        set <- function (y){
                
                x <<- y
                aMatrix <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the inverse of the matrix
        setInv <- function(solve) aMatrix <<- solve
                
        ## get the inverse of the matrix
        getInv <- function() aMatrix
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
}


## The following function computes the Inverse of the matrix returned by the 
## above function. 

## If the inverse has already been calculated, the function should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        ## try to get the matrix
        aMatrix <- x$getInv()
        ## check if the matrix has rows/ records
        if(!is.null(aMatrix)){
                
                ## get the matrix data from the cache
                message("getting cached data")
                return(aMatrix)
        }
        
        ## ELSE
        ## get the matrix
        data <- x$get()
        ## inverse the matrix
        aMatrix <- solve(data,...)
        ## set matrix
        x$setInv(aMatrix)
        ## return the matrix
        aMatrix
}
