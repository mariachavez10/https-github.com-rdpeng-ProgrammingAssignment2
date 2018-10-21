## Put comments here that give an overall description of what your
## functions do
#The makeCacheMatrix function sets the value of the matrix,gets the value of the matrix,
#sets the value of the inverse of the matrix and gets the value of the inverse of the matrix 
#The cacheSolve function takes the result returned by makeCacheMatrix, reviews the inverse of the matrix from
#makeCacheMatrix function. If the inverse of the matrix has already been calculated before, this is, 
#the inverse is not null returns a message "getting cached data", in other case gets the value of matrix,
#calculates the inverse of the matrix with the solve fuction and print it. 
 
## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse
#This function creates a list which containing a function to:
#	1. set the value of the matrix
#	2. get the value of the matrix
#	3. set the value of the inverse of the matrix	
#	4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { #Assign to the matrix the variable x
	inv<-NULL 					#inv starts as null and will save the value of the inverse of the matrix
	set<-function(y) {			#The set function is defined to assign a new value to variable x (matrix)		
		x<<-y 				#use the <<- operator to assign a value to an object 
							#in an environment that is different from the current environment
		inv<<-NULL				#inv restars as NULL  
	}
	get<-function()x				#Get the value of the matrix
	setinverse<-function(inverse) inv<<-inverse 	#Assign a new value to inv 
	getinverse<-function()inv		#Get the value of the inverse of the matrix 
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
#This function calculate the inverse of the special "matrix" returned by makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getinverse()			#Get the inverse of the matrix from makeCacheMatrix function
	if(!is.null(inv)){			#When the inverse has already been calculated,this is,inv is NOT NULL
		message("getting cached data")
		return(inv) 			#Return the previous result of inv 
	}
	Data<-x$get()				#In other case, get the matrix in variable Data
	inv<-solve(Data)				#The inverse of the matrix is calculated with solve 
							#function and saved in the variable inv
	x$setinverse(inv)				#Set the inverse of the matrix 
	inv						#Print the inverse of the matrix
}
