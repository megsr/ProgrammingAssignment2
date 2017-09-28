## Assigment 2 - megsr_13@hotmail.com  Sept 28, 2017

##These functions invert a matrix. They save memory by caching the inverse 
#of the matrix for later use

## the makeCacheMatrix function creates a matrix that can cache its inverse    

makeCacheMatrix <- function(x = matrix()) { #matrix must be invertible
        n<-NULL                             #create initial x and n
        set<- function(y){
                x<<-y                       #assign x
                n<<-NULL                    #clears old runs of cachesolve
        }
        get<- function() x
        setin<-function(solve) n<<-solve
        getin<-function() n
        list(set=set, get=get, setin=setin, getin=getin) 
                                            #gives names to the functions
}

## The function cacheSolve either returns the cached inverse of the
#matrix OR computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        n<-x$getin()
        if (!is.null(n)) {
                message ("getting cached data")
                return(n)
                }
        data<-x$get()        #calculating the inverse if it wasn't cached
        n<-solve(data, ...)
        x$setin(n)
        n                    # Return a matrix that is the inverse of 'x'
}
