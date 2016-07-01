

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




cacheSolve <- function(m=makeCacheMatrix(1:4,nrow=2,ncol=2), ...) 
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
                }
