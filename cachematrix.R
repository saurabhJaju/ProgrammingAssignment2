## create and store a matrix using matrix function in an object
## pass the object to makeCacheMatrix function
## and get output in another object
##now pass this resultant object to the cacheSolve function

## 

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL     #default value
        set<- function(y){
                x <<- y    # cached input matrix
                inv <<- NULL #default value in cache
        }
        get<- function() x      
        setinv <- function(solve) inv <<- solve  #solve function passed 
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        # creates a list to acommodate the functions
        

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){   #checks if inv has previous data
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #get function to get input matrix
        inv<- solve(data,...) #solve function applied to input matrix
        x$setinv(inv)
        inv
       
}
