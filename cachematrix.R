## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special type of matrix, a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL  #This variable will hold the inverse matrix, 
                            #but for now is being set to null, 
                            #since the inverse of the matrix as not been calculated yet
    
    # Setting the value of Vector
    set_vector <- function(y){
        #updating old matrix with new matrix
        x <<- y
        
        #reset inverse_matrix
        inverse_matrix <<- NULL
    }
    
    # getting actual matrix
    get_matrix <- function() x
    
    #set the value of inverse_matrix
    set_inverse_matrix <- function(solve) inverse_matrix <- solve
    
    #get inverse_matrix
    get_inverse_matrix <- function() inverse_matrix
    
    # list with the available functions
    list(set_vector = set_vector, get_matrix=get_matrix, set_inverse_matrix=set_inverse_matrix, get_inverse_matrix=get_inverse_matrix)
    
}

## Working of above function :
# The following function calculates the inverse of the special "matrix" created with the above function (makeCacheMatrix). 
# However, it first checks to see if the inverse of the matrix has already been calculated. 
# If so, it gets the inverse of the matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix of the data and sets the value 
# of the inverse of the matrix in the cache via the setinverse function.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # get the inverse of the matrix
    inverse_matrix <- x$get_inverse_matrix()                    
    
    # check if this inverse of the matrix has been calculated
    if(!is.null(inverse_matrix)) {    
        # if so, prints this message "getting cached data" and
        # returns the inverse of the matrix and skips the computation.
        message("getting cached data")        
        return(inverse_matrix)                       
    }
    
    #If the inverse has not been calculated:
    
    # get the actual matrix
    data <- x$get_matrix()  
    
    # calculating the inverse of the matrix using solve
    inverse_matrix <- solve(data, ...)
    
    # update the variable that holds the inverse of the matrix
    x$set_inverse_matrix(inverse_matrix) 
    
    # return inverse_matrix
    inverse_matrix 
}
