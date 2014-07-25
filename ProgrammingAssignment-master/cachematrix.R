
## makeCacheMatrix creates a list containing a function to 
## 1) set the value of a vector (set)
## 2) get the value of a vector (get)
## 3) set the value of the inverse (setx)
## 4) get the value of the inverse (getx)
 
## set, get, setx and getx are all "public methods", whereas 
## mtx and m are "private members" of makeVector
 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	mtx <- x
	set <- function(y){
		mtx <<- y
		m <<- NULL
	}
	get <- function() x
	setx <- function(inv) m <<- inv
	getx <- function() m
	list(set= set, get= get,
	setx= setx,
	getx= getx)
}


## setx sets the value of the inverse by pulling it from the 
## getx gets the value of the inverse


## This function takes a makeCacheMatrix instance,
## and looks for a matrix called getx (the inverse of the matrix in MakeCacheMatrix)
## if this is not NULL then it has already been calculated and 
## the function will print "getting cached data" and return m (uses getx function)
## if it is NULL, gets matrix and applies the inverse function (solve)
## and returns inverse

cacheSolve <- function(x, ...) {
	m <- x$getx()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setx(m)
	m
}

