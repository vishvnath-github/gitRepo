###### Control Structure ################
# Control structures in R allow you to control the flow of execution of the program, depending on
# runtime conditions. Common structures are

# if, else: testing a condition
# for: execute a loop a fixed number of times
# while: execute a loop while a condition is true



# if(<condition>) {
#   ## do something
# } else {
#   ## do something else
# }

# if(<condition1>) {
#   ## do something
# } else if(<condition2>) {
#   ## do something different
# } else {
#   ## do something different
# }

######## First example 

x=2

if (x>3) {
  y = 20
} else {
  y = 0
}
  
y
 ########## Second example

z = 80

if (z <=100){
  print("The value of z is not equal to 100")
} else {
  print("The value of z is equal to 100")
}

######### Third example
temp = 50

if (temp <= 20){
  print("It's a winter")
} else if (temp > 20 && temp <= 35){
  print("it's a pleasant weather")
} else {
  print("It might be sunny")
}

####### Fourth Example
temp = 25
temp = 20
temp = 35
temp = 40

if (temp  <= 20 || temp >= 30){
  print("it's a pleasant weather")
} else {
  print("It might be sunny")
}

######### Fifth example  
a = 15
b = 5

if (a <= 10){
  print (a*b)
} else {
  print(a+b)
}

######## ifelse - only for two conditions ##############
a =5
ifelse(a <=10, a*b, a+b)

############ Loop ################
####### First example
x = c(4,6,7,8,9,10)

for(i in x) { # i = 5
  print(x[i]) # x[5]
}

### first time value of i = 4 and value of x[4] = 8
### Second time value of i = 6 and value of x[6] = 10
### Third time value of i = 7 and value of x[7] = NA becaue it has nothing 
### Like this fo other value of i

######### Second example
x = c(4,6,7,8,9,10)
for(i in 1:6) {
  print(x[i])
}

### first time value of i = 1 and value of x[1] = 4
### Second time value of i = 2 and value of x[2] = 6
### Third time value of i = 3 and value of x[3] = 7 
#### Like this

###### Third example
w <- c("a", "b", "c", "d")
for(i in 1:4) {
  print(w[i])
}




#### while loop

# while (test_expression) {
#   # block of code to be executed
# }

i <- 0
while(i < 10) {
  print(i)
  i <- i + 1
}

i <- 0
while(i <= 4) {
  print(i)
  i <- i + 1
  print(i)
}

i <- 0
while(i <= 4) {
  print(i)
  i <- i + 2
  print(i)
}

#############  Exercise ######################
############## To check the value of a variable whether it is even or odd #
#### # To obtain the first 12 Fibonacci numbers in R, we can use



