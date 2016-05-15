## Put comments here that give an overall description of what your
## functions do
## These two functions firstly CREATE A MATRIX by computing the inverse of the matrix x, 
## and secondly, RETURN A MATRIX that is the inverse of 'x'

## Write a short comment describing this function
## makeCacheMatrix creates a matrix by using the following functions:
## set the value of the matrix; get the value of the matrix; and cache it inverse
## (via setmatrix and getmatrix)
## Computing the inverse of a square matrix can be done with the solve function in R !!!

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}
## Write a short comment describing this function
## cacheSolve function 

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
