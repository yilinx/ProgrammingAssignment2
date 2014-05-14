### Assignment: Caching the Inverse of a Matrix

##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {    #lets first define the function as "makeCacheMatrix" , and let "x" be the matrix of interest
        m <- NULL    #initialize m as NULL
        set <- function(y) {    #allows the user to redefine a new matrix into makeCacheMatrix; 
                                #eg.    xMat <- matrix(rnorm(16), ncol=4, nrow=4)   #create xMat a 4 by 4 matrix
                                #       xMat_sp <- makeCacheMatrix(xMat)            #let the special list created by makeCacheMatrix be called "xMat_sp"
                                #       xMat <- matrix(rnorm(25), ncol=5, nrow=5)   #update a new xMat to a 5 by 5 matrix
                                #       xMat_sp$set(xMat)                           #update the special list "xMat_sp" with the new 5 by 5 matrix
               
                x <<- y         #updates the matrix stored with new matrix and store this new matrix as a global variable "x"
                m <<- NULL      #set "m" as NULL and define the gloval variable "m" as NULL(to reinitialise the cachesolve process)
        }
        get <- function() x     #retrieve and store matrix "x" into "get"
        setsolve <- function(solve) m <<- solve  #stores the value "solve" in a global variable "m"
        getsolve <- function() m    #retreive the value of global variable "m"
        list(set = set, get = get,   #creates the list containing the elements "set", "get", "setsolve", "getsolve"
             setsolve = setsolve,    #notice that the elements in the list are functions. this shows that list can also
             getsolve = getsolve)    #hold functions in them
        
}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x

cacheSolve <- function(x, ...) {     #to use cachesolve use the special list created using makeCacheMatrix;
                                     #eg. cachesolve(xMat_sp)    #see the example illustrated in "makeCacheMatrix"
                                     
        m <- x$getsolve()   #retrieve the "getsolve" element of the special list and call it "m"
        if(!is.null(m)) {     #performs a check to determine if "m" is NULL. if not NULL, proceed...
                message("getting cached data")  #shows message "getting cached data" if "m" is not NULL
                return(m)    #print "m" and exit function
        }
        data <- x$get()    #if "m" is NULL, retreive the element "get" of the special list and call it "data"
                           #the value stored in "get" is the matrix value stored in makeCacheMatrix
        m <- solve(data, ...)    #solve for the inverse matrix and call this inverse matrix "m"
        x$setsolve(m)            #update the special list by setting the value of "setsolve" as the inverse matrix "m"
        m    #print "m"
}
