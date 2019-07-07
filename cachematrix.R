## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {#function creates a special "matrix" object that can cache its inverse #x is initialized as a function argument, so no further initialization is required within the function
        inverse <- NULL  #set the variable for inverse martix initializing it as an object within makeCacheMatrix
        set <- function(y) { #set() takes this argument - y later:  it doesn't matter whether this argument is called y, aMatrix or any object name other than x
                x <<- y #assigns the value on the right side of the operator to an object in the parent environment
                inverse <<- NULL #clears any value of inverse that had been cached by a prior execution of cacheSolve()
        }
        get <- function() x #R retrieves x from the parent environment
        setinverse <- function(solve) inverse <<- solve #defines the setter for th inverse matrix, <<- is used to assign the input argument to the value of inverse in the parent environment
        getinverse <- function() inverse #retrieve inverse value
        list(set = set, get = get, #returns each of the functions to the parent environment
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  # Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() #retrieve a inverse from the object 
        if(!is.null(inverse)) { #checks to see whether the result is NULL
                message("getting cached data")
                return(inverse) #inverse matrix is returned
        }
        data <- x$get() #if the result in NULL gets the matrix from the input object, calculates a solve()
        inverse <- solve(data, ...) #here not sure in data
        x$setinverse(inverse) #et the inverse in the input object
        inverse    #printing of the inverse martix
        
}
