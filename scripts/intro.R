#https://raw.githubusercontent.com/Neilblund/729A/master/scripts/intro.R

#most of these examples come from http://www.r-tutor.com/r-introduction


#Operations----



#evaulate expressions by typing them directly in to the console: 

1 + 2



#R functions use the syntax "function_name(value)" for instance: the log function will get the natural log of the value in parentheses:

log(10) # log 10

log(2) # log 2

#Variables----

#variable assignment, use the <- or the = sign to assign data to a variable:
x<-1

x

x+2

x<-x+2 

x

#variables are used to temporarily store data. They can be passed to functions, plotted, reassigned, etc.



log(x) #pass x to the log function


#Vectors combine multiple values in to a single element----

# the "c" function combines multiple inputs in to a vector. 

a<-c(2, 3, 5) 

a

#you can perform mathematical operations on an entire vector - e.g. multiply a vector by 5

a<-c(2, 3, 5)

a * 5

#or add a vector to another vector
a + c(1, 2, 3)

#many functions can be applied to an entire vector to return a single value

mean(a)

#all elements in a vector must have the same "type"

c("aa", "bb", "cc", "dd") # string vector

c(TRUE, FALSE, TRUE, FALSE) #logical vector

c(0, 1, 2, 3) #numeric vector




#Vector indexing-----

#use square brackets to retrieve a value at a position in a vector. e.g to get the third value in this vector: 
s = c(2, 1, 0, 5, 4) 

#just use square brackets for position three: 

s[3]


#you can also use a vector of indices to get multiple values from a vector. Get the 1st, 1st, and 3rd values by typing:

s[c(1, 1, 3)]


#use the colon to get all values in a range. Get values 1 through 3 in vector s by typing:

s[1:3]


#you can also use logical vector to subset:

s[c(FALSE, TRUE, FALSE, TRUE, FALSE)] 


#logical subsetting is often used to find values that satisfy some condition: 

s>1  #returns a logical vector which is TRUE if that value is greater than 1, and false otherwise

s[s>1] #returns the values of s that are greater than 1


# named vector members:
v = c("Mary", "Sue")



names(v) = c("First", "Last") 

v

#you can retrieve a named vector member by its name or by its index:

v["First"]
v[1]



#Matrices----

#matrices store data in rows and columns:

A <- matrix( 
  c(2, 4, 3, 1, 5, 7), # the data elements 
  nrow=2,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)  

A


#matrix elements can be accessed by row and column index. The first number is the row index, the second number is the column index

#first row second column
A[1,2]

#second row second column

A[2,2]


#You can also access a whole row or column. e.g : retrieve the whole first row of the matrix
A[1,]


#some functions can be applied to entire matrices. 

t(A) #the t() function returns the transpose of a matrix




#Lists----

#lists can be used to store multiple types of data in a single object.

n  <- c(2, 3, 5) 
s <- c("aa", "bb", "cc", "dd", "ee") 
b <- c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x <- list(n, s, b, 3)   # x contains copies of n, s, b


x

#Lists members are usually accessed with the double brackets: [[]]

x[[1]]

x[[2]]

x[[3]]





#lists can be nested inside other lists

x<-list(n,s,b,list(1, 2, 3))

x

#Data frames----

#Data frames store data in rows and columns (like a matrix), and they can be used to store multiple different data types (like a list)

n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b) 


#You can access columns of a data frame using the $ operator. Access the column named 'n': 
df$n

#You can also access elements by index (similar to a matrix)

df[1,1]



#univariate plots and summaries----

data(mtcars) #use the data function to call up example data sets in R. 


#frequency tables
table(mtcars$gear)

#barplot 

barplot(table(mtcars$gear))

#histogram and density plots

hist(mtcars$mpg)

#kernel density plotting: 
plot(density(mtcars$mpg))


#bivariate summmaries-----

#use the plot function to create scatter plots
plot(mtcars$mpg, mtcars$wt)


#use lm for linear regression
model<-lm(mtcars$wt~mtcars$mpg)

summary(model)


#add a line of best fit to the current plot
abline(model, col="red", lty=2)



#Installing packages, loading libraries, reading external data----

install.packages(ggplot2) #install the ggplot package
require(ggplot2)

protests<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/protest_events.csv") #read a csv from github
protests$date<-as.Date(protests$date)


#create separate line plots for each emotion over time
ggplot(data=protests, aes(date, value)) + geom_line() +  scale_x_date() + xlab("") + ylab("Protests")  + facet_wrap(~variable)
