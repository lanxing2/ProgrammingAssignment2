## Put comments here that give an overall description of what your
## functions do

## 1.Save the Matrix in get and create a list by makeCacheMatrix
## 2.Calculate the inverse of Matrix and save the result by cacheSolve. If the result is already, return the result 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                                        ##set a new matrix 
        x <<- y
        m <<- NULL
    }
    get <- function() x                                         
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'               
    m <- x$getinverse()                                         
    if(!is.null(m)) {                                           ##Judge whether the result exists or not
        #message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)                                            ##calculate the inverse of matrix
    x$setinverse(m)                                             ##save the result
    m
}

#debug by system.time() Compare the time by cacheSolve and calculating inverse matrix in each loop
