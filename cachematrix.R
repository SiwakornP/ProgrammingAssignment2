## Put comments here that give an overall description of what your
## functions to compute inverse matrix and cache those matrix

#This function create a special object, which has 4 different functions to interact with "cacheSolve" function
makeCacheMatrix <- function(mat = matrix() ){
        invs <- NULL
        set <- function(new.mat){
                mat <<- new.mat
                invs <<- NULL
        }
        get <- function() mat
        setinv <- function(cal.invs) invs <<- cal.invs 
        getinv <- function() invs
        list(set = set, get = get, getinv = getinv, setinv = setinv)
}
#This function take special object from makeCacheMatrix to compute the inverse matrix
cacheSolve <- function(S.mat){
        if(!is.null(S.mat$getinv() )){
                print("Cache matrix is used...")
                return(S.mat$getinv() )
        }
        print("Calculate inverse matrix...")
        require(MASS)
        tmp = S.mat$get()
        cal.inv <- solve(tmp)
        print(cal.inv)
        invs <<- cal.inv
        S.mat$setinv(invs)
}
