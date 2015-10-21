## These functions create an inverse matrix and store it to the cache.


## The first function (makeCacheMatrix()) will store an inverse matrix in the
## cache.

makeCacheMatrix <- function(x = matrix()) {

        inverse_matrix <- NULL   #sets the value for inverse matrix NULL locally
        set <- function(y) {     #sets global values for x (the matrix) 
                x <<- y          #and inverse_matrix (NULL)
                inverse_matrix <<- NULL
        }
        get <- function() x      # returns the x (the matrix to be inverted)
        set_inverse <- function(solve) inverse_matrix <<- solve 
        #sets the inverted matrix to the inverse_matrix globally       
        get_inverse <- function() inverse_matrix #returns the inverse_matrix
        list(set = set, get = get,  #makes a list of functions "makeCacheMatrix"
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## This function will check if there is already cached inverted matrix for the
## matrix x, if yes, it returns it, if not it will make the inverted matrix and
## store it.

cacheSolve <- function(x, ...) {
        
                inverse_matrix <- x$get_inverse() 
                #sets the inverse_matrix same as in cache.
                
                if(!is.null(inverse_matrix)) {   
                        message("getting cached data")
                        return(inverse_matrix)
                }
                #This if function checks if there is something in the cache, and 
                #if it's not empty (!is.null), return the value.
                
                data <- x$get()  #sets the data as the matrix to be inverted
                inverse_matrix <- solve(data, ...) 
                #sets the inverse_matrix as inverse of matrix "data"
                x$set_inverse(inverse_matrix)
                #sets the inverse_matrix globally as the inverse of matrix x
                inverse_matrix #return the inverse matrix
}
