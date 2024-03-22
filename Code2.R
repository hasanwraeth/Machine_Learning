# Generate 100 random numbers using a normal distribution # with a mean of 0 and a standard deviation of 1 
random_numbers <- rnorm(100, mean=0, sd=1)
print(random_numbers)

mean <- 5
variance <- 0.1 # Variance
std_dev <- sqrt(variance) # Standard deviation is the square root of variance
# Generate 100 random numbers using a normal distribution with a specified mean␣ ↪and standard deviation
random_numbers <- rnorm(100, mean=mean, sd=std_dev)
print(random_numbers)

# Define x values
x <- seq(-5, 13, length.out = 1000)
# Define the means and standard deviations
means <- c(0, 2, 4, 6, 8)
std_devs <- c(1, 0.5, 0.2, 1.5, 0.3)
# Start plotting
plot(NULL, NULL, type="l", xlim=c(-5, 13), ylim=c(0, 2),
     xlab="x", ylab="Probability Density",
     main="Normal Distributions with Different Means and Standard Deviations")
# Add the normal distribution curves
for (i in 1:length(means)) {
  mean <- means[i]
  std_dev <- std_devs[i]
  lines(x, dnorm(x, mean=mean, sd=std_dev),
        col=i, lty=1, lwd=2) # dnorm calculates the density of the normal distribution
  legend("topright", legend=paste(" =", means, ", =", std_devs), col=1:5, lty=1, cex=0.8)
  }

# Add grid lines
grid(col="black", lty="dotted")

# Parameters
n_trials <- 40
p_success <- 0.5
n_samples <- 100
# Generate random numbers from a binomial distribution
samples <- rbinom(n_samples, size=n_trials, prob=p_success)
# Plotting
hist(samples, breaks=seq(-0.5, n_trials + 0.5, by=1), col="lightgray",border="black",
     main='Histogram of 100 samples from Binomial Distribution',
     xlab='Number of Successes', ylab='Frequency', xaxt='n')
axis(1, at=seq(0, n_trials, by=1))
grid(nx=NA, ny=NULL)

# Define n and p parameter combinations
params <- list(c(20, 0.5), c(30, 0.3), c(40, 0.7), c(50, 0.2), c(60, 0.6))
# Set up the plot
plot(1, type="n", xlab="Number of Successes", ylab="Probability", main="Binomial Distribution PMF for Different n and p Values",
     xlim=c(0, max(sapply(params, "[[", 1))), ylim=c(0, 0.4)) # Set limits for x and y axes
# Add lines for each parameter combination
for (param in params) {
  n <- param[1]
  p <- param[2]
  x <- 0:n
  y <- dbinom(x, size=n, prob=p)
  lines(x, y, type="o", pch=20, lty=1, col=rainbow(length(params))) # Use different colors for lines
  legend("topright", legend=sprintf("n=%d, p=%.1f", sapply(params, "[[", 1),sapply(params, "[[", 2)),
         col=rainbow(length(params)), lty=1, cex=0.8)
  }
# Display the plot
grid()

# Setting the random seed for reproducibility
set.seed(0)
# Lambda value
lmbda <- 5
# Generate 100 random numbers from a Poisson distribution with lambda=5
random_numbers <- rpois(100, lambda=lmbda)
# Print the random numbers
print(random_numbers)

# Setting the random seed for reproducibility
set.seed(0)
# Lambda value
lmbda <- 5
# Generate 1000 random numbers from a Poisson distribution with lambda=5
random_numbers <- rpois(1000, lambda=lmbda)
# Plot histogram
hist(random_numbers, breaks=seq(min(random_numbers) - 0.5, max(random_numbers)+ 0.5, by=1),
     col="lightgray", border="black", main='Histogram of Random Numbers from Poisson Distribution',
     xlab='Value', ylab='Frequency', xaxt='n')
axis(1, at=seq(min(random_numbers), max(random_numbers), by=1))
grid(nx=NA, ny=NULL)

# Setting the random seed for reproducibility
set.seed(0)
# Lambda values
lambdas <- c(2, 4, 6, 8, 10)
# Generate k values (e.g., from 0 to 20 for clear visualization)
k_values <- 0:20
# Set up the plot
plot(0, type="n", xlim=c(min(k_values), max(k_values)), ylim=c(0, 0.3), main="PoissonDistributionPMFforDifferent Values",
     xlab="k", ylab="Probability")
# Plot the PMF for each lambda value
colors <- rainbow(length(lambdas))
for (i in seq_along(lambdas)) {
  lmbda <- lambdas[i]
  probabilities <- dpois(k_values, lambda=lmbda)
  points(k_values, probabilities, col=colors[i], pch=16, type="o", cex=0.8)
  lines(k_values, probabilities, col=colors[i], lty=1)
  }
# Add legend
legend("topright", legend=sprintf(" =%d", lambdas), col=colors, pch=16, lty=1,cex=0.8)
# Display the plot
grid()

# Set the size of the plot in a Jupyter notebook environment
options(repr.plot.width=12, repr.plot.height=8) # Adjust the margins around the plot
par(mar=c(5.1, 4.1, 4.1, 2.1)) # default values: mar=c(5.1, 4.1, 4.1, 2.1)
# Parameters
alpha <- 2
beta <- 5
n_samples <- 100
# Generate random numbers from a beta distribution
samples <- rbeta(n_samples, shape1=alpha, shape2=beta)
# Plotting
hist(samples, breaks=50, col="lightgray", border="black", main='Histogram of 100 samples from Beta Distribution ( =2, =5)', xlab='Value', ylab='Frequency')
grid(nx=NA, ny=NULL)

# Define alpha and beta parameter combinations
params <- matrix(c(2, 5,
                   5, 2,
                   3, 3,
                   0.5, 0.5,
                   2, 8), ncol=2, byrow=TRUE)
# Prepare x values for plotting
x <- seq(0, 1, length.out=1000)
# Set up the plot
options(repr.plot.width=12, repr.plot.height=8)
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(NULL, NULL, type="n", xlim=c(0, 1), ylim=c(0, 10), xlab="Value", ylab="Density",
     main="BetaDistributionPDFforDifferent and Values")
# Plot beta PDF for each alpha and beta combination
for (i in 1:nrow(params)) {
  a <- params[i, 1]
  b <- params[i, 2]
  lines(x, dbeta(x, a, b), col=i, lty=1, lwd=2)
  }
# Add legend
legend("topright", legend=paste(" =", params[,1], ", =", params[,2], sep=""),col=1:nrow(params), lty=1, lwd=2)

# Sample data
sample_mean <- 99
sample_std_dev <- 3
n <- 30
confidence_level <- 0.95
# Calculate the standard error
standard_error <- sample_std_dev / sqrt(n) # Calculate the degrees of freedom
df <- n - 1
# Calculate the t critical value
t_critical <- qt((1 + confidence_level) / 2, df) # Calculate the margin of error
margin_of_error <- t_critical * standard_error
# Calculate the confidence interval
ci_lower <- sample_mean - margin_of_error
ci_upper <- sample_mean + margin_of_error
# Print the 95% confidence interval
cat("95% Confidence Interval: [", ci_lower, ", ", ci_upper, "]\n")

# Sample data
sample_mean <- 99
sample_std_dev <- 3
n <- 30
# Calculate the standard error
standard_error <- sample_std_dev / sqrt(n)
# Calculate the t critical value for a 95% CI 
alpha <- 0.05 # for a 95% CI
t_value <- qt(1 - alpha/2, df=n-1)
# Calculate the half-width of the 95% CI
half_width <- t_value * standard_error 
# Calculate the relative half-width
relative_half_width <- half_width / sample_mean 
# Print the relative half-width
cat("Relative Half-Width:", relative_half_width, "\n")

# Load necessary library
library(stats)
# Define the probabilities and sizes
probabilities <- c(0.1, 0.25, 0.5)
sizes <- c(200, 500, 1000, 2000, 5000, 10000)
iterations <- 5
# Function to calculate the confidence interval
calculate_CI <- function(data, alpha=0.05) {
  sample_mean <- mean(data)
  standard_error <- sd(data) / sqrt(length(data))
  t_value <- qt(1 - alpha/2, df=length(data) - 1)
  half_width <- t_value * standard_error
  CI <- c(sample_mean - half_width, sample_mean + half_width)
  RHW <- half_width / sample_mean
  return(list(CI=CI, RHW=RHW))
  }
# Loop over probabilities and sizes to calculate and print the confidence interval and relative half-width
for (p in probabilities) {
  cat("Probability:", p, "\n")
  for (n in sizes) {
    cat("\tSize:", n, "\n")
    for (i in 1:iterations) {
      # Generate random numbers using binomial distribution
      data <- rbinom(n, size=2500, prob=p)
      # Calculate the confidence interval and relative half-width
      result <- calculate_CI(data)
      cat(sprintf("\t\tIteration %d: CI = (%f, %f), RHW = %f\n", i,result$CI[1], result$CI[2], result$RHW))
    } }
  }

# Load necessary libraries
library(ggplot2)
# Define the probabilities and sizes
probabilities <- c(0.1, 0.25, 0.5)
sim_sizes <- c(200, 500, 1000, 2000, 5000, 10000) 
iterations <- 5
n<-2500 #Noofcases
# Function to calculate the confidence interval
calculate_CI <- function(data, alpha=0.05) {
  sample_mean <- mean(data)
  standard_error <- sd(data) / sqrt(length(data))
  t_value <- qt(1 - alpha/2, df=length(data) - 1)
  half_width <- t_value * standard_error
  return(half_width / sample_mean)
  }
results <- data.frame()
# Loop over different probabilities and sizes and calculate the averagerelative half-width
for (p in probabilities) {
  for (size in sim_sizes) {
    avg_rhw <- mean(sapply(1:iterations, function(.) calculate_CI(rbinom(size,size=n, prob=p))))
    results <- rbind(results, data.frame(Probability=p, SimSize=size,AvgRHW=avg_rhw))
  } }

# Plotting the results using ggplot2
ggplot(results, aes(x=SimSize, y=AvgRHW, color=as.factor(Probability), shape=as.factor(Probability))) +
  geom_line() +
  geom_point() +
  scale_x_log10(breaks=sim_sizes) +
  scale_color_discrete(name="Probability") +
  labs(title='Relative Half-Width of 95% CI vs. Number of Simulations',
       x='Number of Simulations',
       y='Relative Half-Width of 95% CI') +
  theme_minimal() +
  theme(legend.position="bottom")

# Initialization of fixed parameters
cases <- 201
a1 <- 0.59
a2 <- 0.723
a3 <- 0.128
population <- 1019847
n_iter <- 10
# Define alpha and beta values
getAlphaBeta <- function(mu, sigma) {
  alpha <- (((1 - mu) / sigma^2) - (1 / mu)) * mu^2
  beta <- ((1 / mu) - 1) * alpha
  return(c(alpha, beta))
}
true_incidence <- cases / population
# Generate binomial distributed random numbers
set.seed(42) # for reproducibility, equivalent to np.random.seed(0) in Python 
cases_dist <- rbinom(n = n_iter, size = population, prob = true_incidence)
print(cases_dist)

# Compute alpha and beta
alpha1_beta1 <- getAlphaBeta(a1, 0.05)
# Generate random numbers from a Beta distribution 
set.seed(42) # for reproducibility
a1_dist <- rbeta(n_iter, alpha1_beta1[1], alpha1_beta1[2])
print(a1_dist)

alpha1_beta1 <- getAlphaBeta(a1, 0.05) # Computes alpha and beta # Set seed for reproducibility
set.seed(42)
# Generate random numbers from a Beta distribution
a1_dist <- rbeta(n_iter, alpha1_beta1[1], alpha1_beta1[2])

# Compute alpha and beta for a2
alpha2_beta2 <- getAlphaBeta(a2, 0.05) # Set seed for reproducibility
set.seed(42)
# Generate random numbers from a Beta distribution for a2
a2_dist <- rbeta(n_iter, alpha2_beta2[1], alpha2_beta2[2])

# Compute alpha and beta for a3
alpha3_beta3 <- getAlphaBeta(a3, 0.05) # Set seed for reproducibility
set.seed(42)
# Generate random numbers from a Beta distribution for a3
a3_dist <- rbeta(n_iter, alpha3_beta3[1], alpha3_beta3[2])

# Compute the crude rate distribution
crude_rate_dist <- cases_dist / population
print(crude_rate_dist)

# Compute the adjusted rate distribution
adjusted_rate_dist <- crude_rate_dist / (a1_dist * a2_dist * a3_dist)
print(adjusted_rate_dist)

# Set seed for reproducibility
set.seed(123)
# Initialization of fixed parameters
numbers <- c(35, 80, 85)
a1 <- c(0.59, 0.59, 0.59)
a2 <- c(0.989, 0.988, 0.991) #DSH
a3 <- c(0.613, 0.613, 0.613) 
pop_changing <- c(77958, 173878, 588070)
age_group <- c('>2','2-4','5-15')
n_iter <- 100000

# Define alpha and beta values
getAlphaBeta <- function(mu, sigma) {
  alpha <- (((1 - mu) / sigma^2) - (1 / mu)) * mu^2
  beta <- ((1 / mu) - 1) * alpha
  return(c(alpha, beta))
}
incidence <- numeric()
lower_ci <- numeric()
upper_ci <- numeric()
# Calculate adjusted incidence for each year
for (x in 1:length(age_group)) {
  # Define the distribution of cases
  true_incidence <- numbers[x] / pop_changing[x]
  cases_dist <- rbinom(n_iter, size = pop_changing[x], prob = true_incidence)
  # Define the distributions of the adjustment factors
  alpha1_beta1 <- getAlphaBeta(a1[x], 0.05)
  alpha2_beta2 <- getAlphaBeta(a2[x], 0.05)
  alpha3_beta3 <- getAlphaBeta(a3[x], 0.05)
  a1_dist <- rbeta(n_iter, alpha1_beta1[1], alpha1_beta1[2])
  a2_dist <- rbeta(n_iter, alpha2_beta2[1], alpha2_beta2[2])
  a3_dist <- rbeta(n_iter, alpha3_beta3[1], alpha3_beta3[2])
  # Calculate the crude incidence rate
  crude_rate_dist <- cases_dist / pop_changing[x] # Calculate the adjusted incidence rate
  adjusted_rate_dist <- crude_rate_dist / (a1_dist * a2_dist * a3_dist)
  incidence <- c(incidence, mean(adjusted_rate_dist * 100000))
  lower_ci <- c(lower_ci, quantile(adjusted_rate_dist * 100000, 0.025))
  upper_ci <- c(upper_ci, quantile(adjusted_rate_dist * 100000, 0.975))
}
# Plotting the values
plot(1:length(age_group), incidence, type = "l", col = "#8F262A", lwd = 2, xlab= "Age Group", ylab = "Incidence", xaxt='n', ylim=c(0, 200))
axis(1, at=1:length(age_group), labels=age_group)
polygon(c(1:length(age_group), rev(1:length(age_group))), c(lower_ci,rev(upper_ci)), col = adjustcolor("#8F262A", alpha.f = 0.15), border = NA)
legend("bottomright", legend = c("Adjusted Incidence"), fill = c("#8F262A"), inset= .05)
title("Simulation of Typhi Incidence")
