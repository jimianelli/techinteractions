#try out the rmvnorm function in R:
library(mvtnorm)
rmvnorm(n = 10,mean = c(1,1))

#first you need to convert a diagonal correlation table to a symmetrix matrix correlation table

#vector = 
#mydiag = diag(5)
#elements=seq(from = 1,to = 10,by = 1)
#mydiag[lower.tri(mydiag,diag=FALSE)]<-elements #you might be able to get things to this point without any special steps
#mydiag<-mydiag+t(mydiag)-diag(diag(mydiag)) #this is the key line to copy from lower to upper diagonal for a symmetric matrix



#How do you want to read in the information? In mvn format?
#options:
    #Keep in mvn.exe format in case you go back to using that (requires just a small change to the fortran program)
    #Read directly from the .cor file to reduce the number of steps that need to be run in fortran.
    #What is the fewest number of steps right now? - use the par_mvn() and std_mvn() and the other matrix. Can write a function to take these in:

#function to take in same info as for mvn.exe:
#read in mvninputAll.txt:
thefile = "Z:\\GitProjects\\TechInteractions\\Non_Git\\mvninputAll.txt"
stuff = as.numeric(scan(thefile,what = "integer",flush = T,n=2))
nvar = stuff[1]
nreps = stuff[2]

parvec = scan(thefile,nlines = 1,skip = 6)
stdvec = scan(thefile,nlines = 1,skip = 7)
corvec = scan(thefile,skip = 8)
mycor = diag(36)
mycor[upper.tri(mycor)]<-corvec
#This is the symmetric correlation matrix:
mycor<-mycor+t(mycor)-diag(diag(mycor)) #this is the key line to copy from lower to upper diagonal for a symmetric matrix


#then convert the symmetric correlation matrix to a symmetric covariance matrix

#If you know the standard deviations of your individual variables, you can:
#stdevs <- c(e1.sd, e2.sd, e3.sd)
##stdevs is the vector that contains the standard deviations of your variables
#b <- stdevs %*% t(stdevs)  
## b is an n*n matrix whose generic term is stdev[i]*stdev[j] (n is your number of variables)
#a_covariance <- b * a  #your covariance matrix

