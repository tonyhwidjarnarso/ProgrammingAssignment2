## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
    minverse<-NULL
    #substitutes the inputted matrix 'x' with a new matrix in 'y'. Also resets the stored minverse value to 'NULL'
    setmatrix<-function(y){
        mmatrix<<-y
        minverse<<-NULL
    }
    #calls the inputted matrix 'x'
    getmatrix<-function()x
    #sets the value of the cached matrix inverse
    setinverse<-function(inverse)minverse<<-inverse
    #calls the inversed matrix stored in 'minverse'
    getinverse<-function(minverse)
        list(setmatrix=setmatrix,getmatrix=getmatrix,
             setinverse=setinverse,getinverse=getinverse)
}

## Computes the inverse of the special 'matrix' returned by
## makeCachematrix
cacheSolve<-function(x,...){
    #calls the cached matrix and inversed matrix, assigns them to 'matrix' and 'inverse'
    matrix<-x$getmatrix()
    inverse<-x$getinverse()
    #checks whether the value of 'inverse' is not 'NULL'
    if (!is.null(inverse)) {
            message ("getting cached data")
            return (inverse)
        }else{
            inverse<-solve(matrix)
            x$setinverse(inverse)
            inverse
        }
}
