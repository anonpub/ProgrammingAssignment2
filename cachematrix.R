
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y  
      m <<- NULL 
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)   #Naming the list elements is what allows us to use the $ form of the extract operator
}

  
  cachesolve <- function(x, ...) {
    m <- x$getsolve()  
    if(!is.null(m)) {   
      print("getting cached data!!!")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...) #Note that cachesove() is the only place where the solve() function is executed,
    x$setsolve(m)
    m
}
M<-matrix(1:25,nrow=5,ncol=5)
M[1,1]<- 1
M[1,2]<- 0.5
M[1,3]<- 34

M[2,1]<- 0
M[2,2]<- 1
M[2,3]<- 0.5

M[3,1]<- 88
M[3,2]<- 1
M[3,3]<- 1


print("matrix M is: ")
print(M)

print("creating cachematrix object, printing inverse should be null: ")
aMatrix<-(makeCacheMatrix(M))
aMatrix$get()
print(aMatrix$getsolve())           # retrieve the value of m, which should be NULL


print("calling cachesolve to calculate inverse: ")
#aMatrix$set(M)          # reset value with a new vector
print(cachesolve(aMatrix))


print("getting it again ")
print(cachesolve(aMatrix)) # retrieve it directly, now that it has been cached

print("resetting and getting it again to show it's not cached since reset ")
aMatrix<-(makeCacheMatrix(M))
print(cachesolve(aMatrix))
