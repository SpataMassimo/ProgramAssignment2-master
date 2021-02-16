#The following function calculates the inverse of the special "matrix".The first
#function creates a matrix, which by assumption is always assumed to be invertible.
#It contains the methods that allow you to initialize the matrix and its inverse
#to a default value (NULL).
#The second function first checks to see if the inverse has already been calculated.
#If so, it gets the value from the cache and skips the computation. Otherwise,
#it calculates the inverse of the data (matrix) and sets the value (invM) in the 
#cache via the setSolve function.


makeCacheMatrix <- function(x = matrix()) {
  
        invM <- NULL
        
 #Define the set function for the matrix, When the matrix is created, x will 
 #contain the value associated with the variable m, the inverse matrix will 
 #point to a null value (it has not yet been solved)
        set <- function(m){
          
        # Assign the matrix and its inverse to the environment (default value)
                x <<- m
                invM <<- NULL
        }
        
        #Return matrix object
        get <- function() x
        
        # Assign the inverse matrix to the environment
        setSolve <- function(solve) invM <<- solve
        
        #Return invM object
        getSolve <- function() invM
    
        #Return the list of the method
        list( set = set, get=get ,
              setSolve = setSolve,
              getSolve = getSolve )
    
      
}


cacheSolve <- function(x, ...) {
  
        #Return the value of inverse matrix
        invM <- x$getSolve()
        
     #We assume that the matrix supplied is always invertible.If the value of 
     #inverse matrix (invM) is null, The inverse matrix has not yet been solved.
     #Else, it's value is on chache data
        if(!is.null(invM)) {
          
                message("getting cached data")
                return(invM)
        }
        
        #return matrix object
        data <- x$get()
        
        #solve the inverse matrix of x
        invM <- solve(data, ...)
        
        #set the invM object with the value of inverse matrix
        x$setSolve(invM)
        
        invM
}
