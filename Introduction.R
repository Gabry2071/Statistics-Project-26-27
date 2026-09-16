# git and GItHUb are not present in the exams, when we code or modify it we modify the version, git keep taking tracks ofthe version of the code
#GitHub is a website where we share the code, there is a repo on GitHub 
#R can be used  as a calculator

5*12
log(210)
log(10)*exp(23)/sqrt(104)

#we can do a matrix molptiplications the matrix 1 have n row and m columns the matrix2 have m rows and l columns so the result is m rows and l columns
#we can do moltiplication if the columns of matrix are the same as the rows of the second

a = 2
b = 63
c = a*b

matrix(0, nrow=3, ncol=2) 

v1= c(2,4,7,10)
letters = c('a', 'b','c')
#to assign a serires of number from1 to 10
v2= 1:10 

#a vecteor with number to 0 to 1 with a distance 
v3= seq(0,5, by=0.5)

#i want a vector with a repetition of number
v4= rep(1,10)

v5=c(v3,v4)
v5

matrix(0, nrow=3, ncol=2) 
M1=matrix(1:12, nrow=4 , ncol=3) #it start filling by the colums, it can show error if we want to put more elemwnts taht can't be put in the matrix
M2 = matrix(1:12, nrow=4,ncol=3, byrow=TRUE)
M2

v6=c(1,2,3)
v7=c(4,5,6)

#i can bind vector by cbind and bind them by columns, and rbind dy rows
M3=cbind(v6,v7)
M3

#t(matrixname) will do a transposition column and row are inverted

is.matrix(v6)
v6=as.matrix(v6)
is.matrix(v6)
v6

length(v6)

v6[1:2] #acces the elements to one to two
v6[c(1,3)] #to acces an element i use the index

M3
M3[2, 2]
M3[1:2,2] #elements 1 and 2 of the second row

M3[1,]# to show a row

ls()
rm(M3)
rm(list=ls())

a=1
b=5
c=c(2,3,4)
d= rep(10,3)

e=1:3
f= rep(10,5)

e+f #r recycles if the vector are not long enough

#vector plus a scalant it add the scalar to each elements of the vector
a+f

M1= matrix(1:12, nrow=4, ncol=3)
M2=rbind(rep(0,3), 1:3, 10:12, c(4,7,12))
M2
M1 %*% M2 #try to look at the matrix dimension
dim(M1)
dim(M2)
M2=t(M2)
M1%*%M2

#i want a sum of elemnts of a matrix for rows and columns
colSums(M1)

a
bool1= a==1
bool1
a==1
a!=2
a==2
a<0
#booleans are helpful if i have to do something if a condition is fullfilled
if(a>1){
  cat("ciao")
}else{
  cat("bye")
}

e
g=c(e,f)
g

for(i in 1:10){
  cat("ciao\n")
  cat(i)
}

for(i in 1:length(g)){
  g[i]=g[i]-1
}
g

#data.frames

exam= data.frame(id=c(123,456,789), written=c(10,30,32), projects=c(30,27,25))
exam

ID=exam$id
ID
attach(exam)
id 
detach(exam)
#graphics
x=0:3
x
y= c(4,5,2,10)
plot(x,y, type= "l", col= "blue")

quartz()
y2=c(1,2,3,4)
points(x,y2)

#build your own function like heaviside_step_function 
heaviside_step_function=function(x){
  if(x<0) return(0)
  return(1)
}

heaviside_step_function(0)
heaviside_step_function(-43)

#to debug the code we usa a function called browser
heaviside_step_function=function(x){
  browser()
  if(x<0) return(0)
  return(1)
}
heaviside_step_function(3)

