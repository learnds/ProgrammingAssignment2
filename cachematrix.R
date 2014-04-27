## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		## Initialize the return object m to null upon function invocation
		m <- NULL
		
		## set the value of the global object x using the set function. 
		
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
		## Use get to gfetch/return the value of the object x 
        get <- function() x
		
		## Set the inverse of the matrix object. 
		## Note: This function is invoked by the cacheSolve function below
        setinv <- function(invmat) m <<- invmat
		
		## Fetch/Return the value of the inverse of the matrix
        getinv <- function() m
		
		## Return a list containing functions to manipulate the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
			
}

## Write a short comment describing this function
## This function computes the inverse of the matrix returned by the makeCacheMatrix.
##		If the inverse is already calculated and the matrix has not changed, 
##		then it returns the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## Check if the matrix has changed first
	        
		mat<-x$get()
		
		## Check if an old value exists and the dimensions are same
		if (exists("oldmat") && dim(oldmat)==dim(mat)) {
			
			## Check if matrix has changed
			z<-apply(oldmat==mat,1,all)
			
			if (length(z[z==FALSE]) == 0) {

				## Now check if the inverse has been computed before
				m <- x$getinv()
			
				## return the value if it is computed before (not null)
				if(!is.null(m)) {
						message("getting cached data")
						return(m)
					}	
			}
		}
		## if the matrix has changed, update the global variable oldmat and compute the inverse
		
		oldmat <<- mat
		message("Computing matrix inverse")
		m <- solve(mat)
        x$setinv(m)
        m
}
