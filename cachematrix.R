## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mat=matrix()){
	inv<-NULL
	set <- function(y){
		mat<<-y
		inv<<-NULL
	}
	get <- function() mat
	setinv <- function(sol)  inv<<-sol
	getinv <- function() inv
	list(set = set, get=get,setinv=setinv,getinv=getinv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mat,...){
	inv<-mat$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- mat$get()
	inv <-solve(data,...)
	mat$setinv(inv)
	inv #return the inverse matrix
}

# Test
test.data <- matrix(c(-1, -2, 1, 1), 2,2)
temp <- makeCacheMatrix(test.data)
result <- cacheSolve(temp)
result
result <- cacheSolve(temp)
result
