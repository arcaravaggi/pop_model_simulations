# https://kevintshoemaker.github.io/NRES-470/LAB1.html#procedure

# https://rstudio-pubs-static.s3.amazonaws.com/252603_9c6a71110dc74cc7832f154449235f7f.html#calculating-lambda-from-counts

# https://sites.google.com/site/wild8390/home/spring-2019/lecture-lab-schedule/week-1-introduction/population-models-in-r


r <- 0.1     #Assign the value of 0.1 to the object "r", or per-capita growth rate (discrete)
lambda <- 1+r
N0 <- 100    #Assign the value of 100 to the object "N0", or initial population size
nyears <- 30 #Assign the value of 30 to the object "nyears", or the number of time steps to simulate


years <- seq(from=0, to=nyears, by=1)   #Creates a sequence of numbers from 0 to the value stored in the object "nyears" (in this case, 50). Because you've told this sequence to increment by 1, you've created a string of numbers from 0 - 50 that contains 51 elements. A single series of elements (e.g., a single column of numbers) is called a vector. You then assign this vector to the object "years".


N <- numeric(nyears+1)    #Make an empty storage vector. The numeric() function takes the contents within the parentheses and converts those contents to the "numeric" class. Don't worry if this doesn't make sense -- what you need to know is that the value within the parentheses (in this case, 50+1=51) is used to tell this function how many zeros to create. So, this line of code creates a vector of 51 zeros, and assigns that vector to the object "N".

N[1] <- N0                # The brackets [] are used to indicate the position of an element within a vector. This line of code assigns the value of the object "N0" (100) to the first element in the "N" object. Remember, the "N" object is a vector of 51 zeros. Now, the first zero is changed to 100.
lambda <- 1 + r           # (1 + r) is equal to lambda, the finite rate of growth.  This stores the result of the calculation (1 + 0.1 = 1.1) in the object "lambda".
for (i in 2:(nyears+1)){  # This for-loop will run through the line of code between the curly brackets {}. "i" is simply the name of a variable (you can use "j", or "k", instead -- any variable name will do). "i" changes each time the loop iterates; basically, it will increase by 1 each time the loop is run, starting at "2" up until the specified maximum number of loops "50+1". 
  N[i] <- lambda*N[i-1]   # This takes the [i - 1] element of "N", multiplies that element by the value of lambda, then assigns that calculated result to the [i] element of "N".
}  


plot(N~years) 

growth <- function(r = 0.1, population = 100, time_steps = 50){
  years <- seq(from = 0, to = time_steps, by = 1)
  N <- numeric(time_steps+1)
  lambda <- 1+r
  N[1] <- population
  for(i in 2:(time_steps+1)){
    N[i] <- lambda*N[i-1]
  }
  cbind(N, )
  return(N)
}

r <- growth()

