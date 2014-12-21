## Function makeCacheMatrix takes an input matrix and creates an object of type list which 
## contains the original and the inverse matrix (initially set to NULL) and functions to get 
## or set both matrixes.
## Function cacheSolve returns the inverse of an input matrix, that has been fetched to makeCacheMatrix  
## by calculating the inverse or getting it from cached value if it has been calculated before.


## Function makeCacheMatrix takes an input matrix and creates an object of type list.  This object 
## stores the original matrix and what will be the cached inverse value of the original matrix, 
## which is initially set to NULL. In the list object there are 4 functions, 2 to read - get and 2 
## to change - set (the original matrix or the inverse matrix). This functions are called by another 
## function cacheSolve.


makeCacheMatrix <- function(x = numeric()) { #input x will be a matrix
    i <- NULL		#set inverse i to NULL whenever makeMatrix is called
    set <- function(y) {	#y is the input matrix
        x <<- y			#save input matrix y
        i <<- NULL}		#inverse matrix i is set to NULL, when a new object is generated
    get <- function()		#function that returns the value of the original matrix 
        { x }
    setinverse <- function(solve)	#called by 
        { i <<- solve }
    getinverse <- function() 	#
        { i } 
    list(set=set, get = get, 	#create a list of methods set,get,setinverse,getinverse
         setinverse = setinverse, getinverse = getinverse) 
}



## Function cacheSolve access the object created with makeCacheMatrix by fetching the value of the 
## matrix used to create the object. If the inverse of the matrix has not been calculated yet (it 
## is still NULL) cacheSolve calculates the inverse and stores it in the object created by the call 
## of makeCacheMatrix and returns the inverse. If the inverse has been calculated earlier then 
## cacheSolve fetches it and returns the inverse value, saving the computing time required to 
## calculate the inverse again. Function always returns tne inverse.


cacheSolve <- function(x, ...) {	#input x is the object created by makeMatrix
	i <- x$getinverse()	#call function getinverse from object x and get the inverse matrix
	if(!is.null(i)) {	#if the inverse matrix is not NULL
		message("getting cached data")	#display this message on the console
		return(i)			#return the inverse to the console and end cacheSolve
	}					
	data <- x$get()		#if i is NULL get from object x to get input matrix
	i <- solve(data, ...)	#calculate the inverse of input matrix and store it in i
	x$setinverse(i)		#store the calculated inverse i in object x 
	i			#returns the inverse matrix i
}
