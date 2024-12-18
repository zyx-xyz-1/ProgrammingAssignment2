##      The following two functions are used to create and cache the inverse 
## matrix of any given invertible matrix. In this way, it avoids repeating
## to calculate the inverses of matrices.

##      For the first function, "makeCacheMatrix", it will return a list including
## different functions, which creates the new matrix and allows users to set their 
## matrices and get the inverse form of their own matrices.
## 1. set the matrix,
## 2. get the matrix,
## 3. set the inverse of the matrix,
## 4. get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inver<- NULL
        set<- function(y){
                x<<- y
                inver<<- NULL
        }
        get<- function() x
        set_solve<- function(solve) inver<<-solve(x)
        get_solve<- function() inver
        list(set = set, get= get, 
             set_solve = set_solve,
             get_solve = get_solve)
}


##      In the second function, it calculates the inverse of the matrix created 
## by the first function. Before the calculation, it first examines whether the 
## inverse matrix has been calculated and recorded in the first function. If so,
## it will return the matrix and leave the message that "getting cached inverse 
## matrix". Otherwise, it will calculate the inverse matrix and cache it.

cacheSolve <- function(x, ...) {
        inver<- x$get_solve()
        if(!is.null(inver)){
                message("getting cached inverse matrix")
                return(inver)
        }
        matrix<- x$get()
        inver<- solve(matrix,...)
        x$set_solve(inver)
        inver
}
