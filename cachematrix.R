## This function creates a list of functions to: 1) set a matrix, 2) get the matrix, 3) set the inverse,
##  and 4) get the inverse.  

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
 	set <- function(y=matrix()) {
	        x << y           ## (re)assign x in parent env so "get" fn will return the correct (updated) matrix.
	        inv << NULL      ## (re)assign inv in parent env so the inverse will be recalculated if the matrix changes.
	}
	get <- function() x          			## returns matrix x (found in the parent environment)
	setinv <- function(inverse)  inv << inverse     ## assign inverse (found in parent environment) to cache
	getinv <- function() inv     			## returns the inverse of matrix x (found in parent environment)
	list(set=set,get=get,setinv=setinv,getinv=getinv)

}

## This function calculates the inverse matrix of the list created with the makeCacheMatrix function, and caches it 
## for future use. It checks first to see if the inverse aleady exists, and if so, uses that.   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## checking to see if inverse already exists	
	inv <- x$getinv(x, ...) {
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)            ## if matrix exists, it will be returned and execution stops.
	}

	## calculating inverse if it doesn't already exist
 	matdata <- x$get()		## retrieves the matrix that was set via makeCacheMatrix function
	inv <- solve(matdata, ...)      ## calculates the inverse
	x$setinv(inv)           	## caches inverse
	inv

}
