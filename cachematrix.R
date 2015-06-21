#Assignment 2 
#purpose: to understand an R function is able to cache potentially time-consuming computations.
#To write a pair of functions that cache the inverse of a matrix.

## the function:makeCacheMatrix was created to set up 
#a special matrix the can calculate inverse matrix and store the saved cache which can be returned in order to
# save the repeating calcualtion time for the matrix inverse.
# the MakecacheMatrix creates a special "matrix",which is really a list 
#containning a function to 
#1,set the value of the matrix
#2,get the value of the matrix
#3,set the value of the inverse
#4,get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        #set is a function that changes the matrix store in the makeCacheMatrix
        set<-function(y){
                x<<-y # <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.
                i<<-NULL
        }
        get<-function() x
        setinverse<- function(inverse) i <<-inverse #store the value of the input in "i"
        getinverse<- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      #function list() to store 4 founctions
        
}


## the cacheSolve function calculates the inverse of the matrix 
# which created within the makeCacheMatrix funciton

cacheSolve <- function(x, ...) {
        i<-x$getinverse() #check to see if the inverse has been calculated by makeCacheMatrix function
        if(!is.null(i)){
                message("getting cached data") #if there is a inverse value, 
                #it gets the inverse from the cache and skips the computation.
                return(i)
        }## Return a matrix that is the inverse of 'x'
        data<-x$get()
        i<-solve(data, ...) #calcuate the inverse of the matrix
        x$setinverse(i)#store the value of inverse matrix in the cache
        i
        
}
