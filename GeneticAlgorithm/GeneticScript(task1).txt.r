library(GA)
# the library instruction loads a package and should be used only once during one program
# running
#setting the parameters
min_x = -5
max_x = 5
pop_size = 10
pc = 0.8
pm = 0.1
maxiter = 5
sleep = 2
#sleep=a – a is delay in seconds before the next generation is to be displayed, if a=-1 then the
#next population is shown by hitting any key (e.g. enter)
#the function to optimized
f <- function(x) abs(x)-cos(3*x)
#fitness function, the same as the optimization one if it is to be maximized, the negation of the
#optimization one if it is to be minimized
fitness = function(x) abs(x)-cos(3*x)
#drawing the function graph
5
curve(f, min_x, max_x)
# monitoring function showing succeeding steps of the algorithm
monitor <- function(obj)
{
curve(f, min_x, max_x, main = paste("Generation =", obj@iter))
points(obj@population, f(obj@population), pch = 1, col = 2)
rug(obj@population, col = 2)
if (sleep<0)
readline()
else
Sys.sleep(sleep)
}
# the algorithm running
GA <- ga(type = "real-valued",fitness = fitness,min = min_x,max = max_x,popSize = pop_size,
pcrossover = pc,pmutation = pm,maxiter = maxiter,monitor = monitor,keepBest = TRUE ,seed =
123)
#displaying the summary
summary(GA)
# showing the best fit individual on the function graph
abline(v = GA@solution, lty = 3)
#displaying the fitness if the best individual in each generation
GA@bestSol
