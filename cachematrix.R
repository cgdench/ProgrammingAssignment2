##  Calculating the inverse of a matrix can be time consuming when carried out
##  on a large scale.
##  The following code endeavours to reduce the impact of such an undertaking
##  through avoiding the repetition of identical computations.
##  The following functions, when prompted with an argument (in the form of a 
##  matrix) can check whether or not the inverse for that matrix has already 
##  been computed. If it has then that data can be accessed. On the other hand, 
##  if it does not already exist in the cache then it will compute the inverse 
##  and cache it for future reference.

## Two main composites 'makeCacheMatrix' and 'cacheSolve' make up the code:

## 'makeCacheMatrix' is a function that stores a matrix and which can cache 
##  it's inverse. 
##  It takes the matrix as it's argument and returns a list of functions;

##  - The 'set' function takes an argument 'y' and passes it to another 
##      environment where it is cached as 'x'.
##  - The 'get' function simply returns the current value of x. ie. It returns
##     the matrix that was cached in 'set'.
##  - The 'setinverse' function is passed the inverse of the matrix from within
##      the cacheSolve function.
##  - The 'getinverse' function simply returns the value of 'm'. ie. The 
##      inverse matrix that was cached in 'setinverse'.

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function () invmat
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
    
}

## The following function has the ability to compute the inverse (using the 
## 'solve' function) of the "special matrix" returned by 'makeCacheMatrix' 
## above. However, first it will check whether the inverse has already been 
## calculated.
## If the inverse has already been calculated then 'cacheSolve' will retrieve 
## it from the cache. This can be seen within the if function.
## Either way 'cacheSolve' ends by returning the inverse matrix.

cacheSolve <- function(x, ...) {
        invmat <- x$getinverse()    
        if(!is.null(invmat)) {  
        message("Getting cached data.") 
            return(invmat)  
        }
        data <- x$get()
        invmat <- solve(data, ...)  
        x$setinverse(invmat)
        invmat
    
}

## To test the code simply copy and then paste it into a console, which is R
## compatible and pass it some suitable arguments.
## eg. 'M1$set(matrix(1:9, 3, 3))' or 'M2$set(matrix(c(1, 7, 7, 4), 2, 2))'
## Then once a matrix has been set other functions can be explored. Some things
## you can try are:
## 'M1$get()'
## 'M1$getinverse()'
## Then if 'NULL' is returned you may want to pass the command 'cacheSolve(M1)'
## What does the command 'M1$getinverse()' return now?

## NB: The matrix you pass as the initial argument must be a square matrix.

## Thanks for your time.