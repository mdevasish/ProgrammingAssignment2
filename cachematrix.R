
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is the function with set of functions that help to scalculate the inverse function.
makeCacheMatrix <- function(x = matrix())
                   {
                   cinv<-NULL
                   set<-function(insert=matrix()){
                   x<<-insert
                   cinv<<-NULL
                   }
                   get<-function()x
                   setinv<-function(inv) cinv<<-inv
                   getinv<-function() cinv
                   list(set=set, get=get, setinv=setinv, getinv=getinv)
                 }


##cacheSolve function is the function which calculates the inverse of a matrix. If the inverse is already calculated the inverse is returned back.

cacheSolve <- function(m, ...) 
                {
                calinv<-m$getinv()
                if(!is.null(calinv))
                  {
                    message("getting cached data")
                    return(calinv)
                   }
                data<-m$get()
                calinv<-solve(data,...)
                m$setinv(calinv)
                calinv
                }
