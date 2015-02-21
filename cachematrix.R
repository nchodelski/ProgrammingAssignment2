##Assignment 2 R: peer review: Caching the Inverse of a Matrix

# These functions, given a square matrix (equal dimensions), save the matrix information, and then calculate and return the inverse of the matrix. 
#The original matrix and it's inverse are both cached outside of the functions' environment, to ensure that the values of the two matricies are preserved beyond the current instance of the function. 
     


     
#About the makeCacheMatrix function:  This function creates a special matrix object (matrix) to store the inverse matrix it calculates  It also creates an object (x) to store the original matrix that is inputted into the function.  
#Both of these objecs are cached outside of the function's environment with the "set" function.  
#The "get" function returns the value of the matrix, when called by the next function, cacheSolve.  
#The "setmatrix" and "getmatrix" functions cache and retreive the cached value of inverse matrix, respectivly.

     
makeCacheMatrix <- function(x = matrix()) {  #pass in matrix instead of x vector
     matrix <- NULL # inverse matrix : where mean would be stored
     set <- function(y) {  #stores the matrix and its inver
          x <<- y   # storing orignal matrix x
          matrix <<- NULL   #storing space inverse matrix in upper enviro
     }
     get <- function() x  #retrieves x the original matrix
     
     setmatrix <- function(m) matrix <<- m  #sets the 
     getmatrix <- function() matrix  #called by cache solve,  returns the inverse matrix, 
     #but if inverse matrix has not been calculated yet, then it goes on to calculate after getting data
     
     
     list(set = set, get = get,  #list of functions i guess...because they are only defined in this function
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}




#The cacheSolve function calculates the inverse of the matrix object (x) created with the previous function. 
#The cacheSolve function first checks whether the inverse matrix has already been calculated. 
#If it has already been solved, then the function retrieves the inverse matrix value from the cached "matrix" and returns this. #Otherwise, it now calculates the invserse of the matrix and then sets the value of "matrix" in the cache using the "setmatrix" function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     matrix <- x$getmatrix() #gets inverse matrix using function of object X provided by prev function
     if(!is.null(matrix)) {  #if inverse matrix value is not null it rereives the value from upper enviro
          message("getting cached matrix data")
          return(matrix) #return breaks function i think
     }
     data <- x$get() # If inverse matrix value is null, then it gets the vector from before get() of object
     matrix <- solve(data, ...) #XXX this step must calculates the inverse matrix and stores as matrix variable
     x$setmatrix(matrix)   #and so now matrix is cached, so before it returns the matrix, it sets the inverse matrix to mean in the upper enviro
     matrix #returns m
}

