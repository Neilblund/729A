

#looping----

# a while loop will repeat until some condition is met. This one prints letters until i passes 26
i<-1
while(i <=26){
  print(letters[i])
  
  #this increments i each time we run the loop. Leaving this out would create a loop that ran forever.
  i<-i+1
}

#a for loop will perform some operation on each value in a list. This one prints i for every value in 1:26. 

for(i in 1:26){
  print(letters[i])
  # i increments automatically in a for loop, and it starts back at 1 each type we run the loop. 
}

#use a for loop to store results from multiple runs of data

#empty matrix with one row
my.data<-matrix(NA, nrow=100, ncol=1)
for(i in 1:100){
  my.data[i]<-rnorm(1)
}
sum(my.data)


#function in a loop----
myFunc<-function(n){
  a<-mean(rnorm(n))
  return(a)
}


my.data<-matrix(NA, nrow=100, ncol=1)
for(i in 1:100){
  my.data[i]<-myFunc(100)
}

hist(my.data)

#loop in a function

myFunc<-function(n, repetitions){
  my.data<-matrix(NA, nrow=repetitions, ncol=1)
  for(i in 1:repetitions){
    random<-rnorm(n)
    my.data[i]<-mean(random)
  }
  return(my.data)
}


plot(density(myFunc(5, 1000)), xlim=c(-1, 1))
plot(density(myFunc(100, 1000)), xlim=c(-1, 1))

#R also provides an apply family of functions
#These are (supposedly) more computationally efficient than loops. They are also (indisputably) a quicker and cleaner looking way to write code. 

#sapply and lapply will take a data frame or a list and apply a function to each element in that list or data frame.

#lapply will return a list
lapply(rep(5, 5), function(x) rnorm(x))

#sapply will try to simplify and return an array or matrix where possible
sapply(rep(5, 5), function(x) rnorm(x))


#replicate function - a wrapper for sapply that offers simpler syntax for repeating a task N times
replicate(100, rnorm(1))


#a shorter version of the previous CLT function 
myFunc<-function(n, repetitions){
  return(replicate(repetitions, mean(rnorm(n))))
}

plot(density(myFunc(5, 1000)), xlim=c(-1, 1))
