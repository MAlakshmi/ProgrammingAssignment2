## Put comments here that give an overall description of what your
## functions do

#Write a short comment describing this function

#  create a matrix for which inverse is to be determined
makeCacheMatrix <- function(x = numeric(),n=2) 
{
    x<-matrix(x,n,n)
    m<-NULL                           #  inverse matrix is initially set to NULL
    get<-function()     { x }         #  returns the input matrix 
    set<-function(y)                  #  change the input matrix to new value
        {  
           y<-matrix(y,n,n)
           x<<-y 
           m<<-NULL
        }
    setinv<-function(mean)  {  m<-mean  }    #  sets the inverse to input parameter
    getinv<-function()  {  m  }              #  returns the inverse of the matrix
    list(get=get,set=set,setinv=setinv,getinv=getinv)   # the function names are created as list

}


## caches the inverse of given matrix

cacheSolve <- function(x, ...) 
{
 ## Return a matrix that is the inverse of 'x'
    m<-x$getinv()
    if (!is.null(m)    # tests if the inverse is already available in cache and return if available
         {
             message("getting from cache")
             return(m)
         }
    data<-x$get()         # if not available, get the new matrix 
    m<-solve(data,...)    #  caluculate the inverse
    x$setinv(m)           # store the inverse in m
    m                     # m is returned from the function
}
