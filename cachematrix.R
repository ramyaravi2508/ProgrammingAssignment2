##cacheSolve is a funtion that will return the inverse of the matrix that is inputted
##to the makeCacheMatrix function. If the inverse of the matrix is already computed, 
##then the result is retrieved from the cache, otherwise is computed. 


##makeCacheMatrix is a function, which will take a numeric matrix 'x' as input, 
##and returns the list of all the functions defined in it(which can be performed on the matrix)

makeCacheMatrix <- function(x =matrix()){
        ## initialize inv to NULL, eventually we will have inv to save the inverse of matrix
        inv<-NULL
        
        ## set is a function which would set the input to makeCacheMatrix to the matrix y ,
        ##and resets the  inverse of previous matrix to NULL
        set<-function(y){                
                x<<-y
                inv<<-NULL        
        }
        
        ##returns the matrix X thats passed to the function makeCacheMatrix
        get<-function(){                
                x                
        }
        
        ##sets the inverse of the matrix 
        setInv<-function(invmat){
                inv<<-invmat
        }
        
        ##retrieves the inverse of the matrix 
        getInv<-function(){
                inv
        }
        
        list(set=set,get=get,setInv=setInv,getInv=getInv)
}



## cacheSolve takes the list(output from makeCacheMatrix) as input, 
##and returns the evaluated inverse of the matrix inputted to makeCacheMatrix 
## Returns the cached inverse if the inverse has already been computed.
cacheSolve<-function(x,...){
        
        ##assign the inverse of the matrix inputted to makeCacheMatrix. 
        ## initialized to NULL if the inverse is not computed
        inv <- x$getInv()
        
        ##Checks if the inverse has already been computed, if its computed before, 
        ##then value from the cached matrix inverse is returned 
        if(!is.null(x$getInv())){
                message("Retrieving the cached matrix-inverse ")
                return
        }
        
        
        data <- x$get()
        ##Checks if the input matrix is not a square matrix, 
        if(nrow(data) != ncol(data)){
                message("The matrix should be a square matrix to determine the inverse of it")
                return
        }
        else
        {
                ##Checks if the determinant of the input square matrix is equal to 0 , 
                ##then matrix inverse is not computed                
                if(det(data) ==0)
                {
                        message("Inverse of the input matrix cannot be determined as the determinant is 0")
                        
                }
                else
                {
                        ## Determine the inverse of the inputted invertible matrix        
                        inv <-solve(data)
                        x$setInv(inv)
                }
        }
        
        inv
}