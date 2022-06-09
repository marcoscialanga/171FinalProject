# install.packages("Pareto") if necessary

# Code for the plot in section 3.1
# Note: I repeated the experiment a few times before choosing a plot that looked 
# "good", i.e., neither ruin occurred very early nor the surplus blew up.
t <- 0
T_values <- c(0)
U <- 100
U_values <- c(U)
S <- 0

while (t < 300) {
  tau <- rexp(n = 1, rate = 1/10)
  T_values <- c(T_values, rep(tau + tail(T_values, n = 1),2))
  x <- rexp(n = 1, 1/20)
  U1 <- U + 1.5*tau
  U_values <- c(U_values, U1)
  U <- U + 1.5*tau-x
  U_values <- c(U_values, U)
  t <- t + tau
  if (U <= 0) {
    break
  }
}

plot(T_values, U_values, ylab = "Surplus", xlab = "Time", 
     main = "Simulation of Cramér-Lundberg Model, Exponentially Distributed Claims", 
     ylim = c(min(c(0,U_values))-10,max(U_values)+10), type = "b", pch = c(1,2))
abline(h = 0, col = "red")

# Code for section 3.2
library(Pareto)
set.seed(1) # For reproducibility
lambda <- 10
alpha <- 2.2
x0 <- 2000
c <- 2000
r <- 0

cat("The mean of a Pareto distribution with alpha and x0 as above is: ",
    alpha*x0/(alpha-1), ".", sep = "")
cat("The relative security loading is then: ", (c/3666.67*lambda)-1, ".",  sep = "")
cat("The ruin probability with no initial capital should be: ", 
    1/(1+(c/3666.67*lambda)-1))

u_vec <- seq(0, 10000, by = 200)
rp_vec <- c()

# This for loop takes a while to run
for (i in seq_along(u_vec)) {
  r <- 0
  for (j in 1:1000) {
    t <- 0
    U <- u_vec[i]
    while (t < 10000) {
      tau <- rexp(n = 1, 1/lambda)
      x <- rPareto(n = 1, t = x0, alpha = alpha)
      U <- U + c*tau-x
      t <- t + tau
      if (U <= 0) {
        r <- r + 1
        break
      }
    }
  }
  rp_vec <- c(rp_vec, r/1000)
}

rp_vec
plot(u_vec, rp_vec, main = "Estimation of Ruin Probabilities as Initial Capital Increases",
     ylab = "Ruin Probability", xlab = "Inital Capital")

