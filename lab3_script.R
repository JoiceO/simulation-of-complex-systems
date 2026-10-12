library(ggplot2)

# CLT Simulation for Exponential Distribution
set.seed(123)

lambda <- 2 # Rate parameter
n_sims <- 1000
sample_sizes <- c(5, 10, 30, 100)

# Function to compute sample means
compute_sample_means <- function (n, n_sims, lambda) {
  replicate(n_sims, mean(rexp(n, rate = lambda)))
}

# Run simulations
par(mfrow = c(2, 2))

for (n in sample_sizes) {
  means <- compute_sample_means (n, n_sims, lambda)
  hist(means, probability = TRUE,
       main = paste("n =", n),
       xlab = "Sample Mean",
       breaks = 30,
       col = "lightblue")

     # Theoretical Normal approximation
 mu <- 1/lambda
 sigma <- 1/lambda
 curve(dnorm(x, mean = mu , sd = sigma/sqrt(n)),
       add = TRUE, col = "red", lwd = 2)

 legend ("topright", legend = "CLT approx",
         col = "red", lwd = 2)
}

# Reset layout
par(mfrow = c(1, 1))

# Compute variance of sample means vs theoretical
n_values <- seq(10, 500, by = 10)
var_means <- sapply(n_values, function(n) {
  means <- compute_sample_means(n, 500, lambda)
  var(means)
})

theoretical_var <- (1/lambda^2) / n_values

# Plot comparison
plot(n_values, var_means, type = "l",
      xlab = "Sample Size", ylab = "Variance of Sample Means",
      main = "Variance of Sample Means vs Theoretical")
lines(n_values, theoretical_var , col = "red", lwd = 2)
legend("topright", legend = c("Empirical", "Theoretical"),
        col = c("black", "red"), lwd = c(1, 2))

########################################################
#Exercise 2: CLT for Different Distributions
#Goal: Test the CLT for non-Normal distribution
# CLT for Poisson distribution
lambda_pois <- 5
sample_sizes <- c(5, 10, 30, 100)

par(mfrow = c(2, 2))

for (n in sample_sizes) {
  means <- replicate(1000, mean(rpois(n, lambda = lambda_pois)))
  hist(means, probability = TRUE,
       main = paste ("Poisson, n =", n),
       xlab = "Sample Mean",
       breaks = 30,
       col = "lightblue")

  # Theoretical approximation
  curve(dnorm(x, mean = lambda_pois,
                sd = sqrt(lambda_pois/n)),
       add = TRUE, col = "red", lwd = 2)
}

par(mfrow = c(1, 1))

# CLT for Uniform distribution
par(mfrow = c(2, 2))

for (n in sample_sizes) {
  means <- replicate (1000, mean(runif(n, min = 0, max = 1)))
   hist(means, probability = TRUE,
          main = paste("Uniform, n =", n),
          xlab = "Sample Mean",
          breaks = 30,
          col = "lightblue")

  curve(dnorm(x, mean = 0.5,
                 sd = sqrt(1/12/n)),
         add = TRUE, col = "red", lwd = 2)
}

par(mfrow = c(1, 1))

#########################################################################
# Exercise 3: Maximum Likelihood Estimation
# MLE for Normal mean (known variance)
set.seed (123)
x <- rnorm(100, mean = 5, sd = 2)

# MLE is the sample mean
mle_mean <- mean(x)
cat("MLE for mean:", mle_mean, "\n")

# MLE for Normal mean and variance
mle_sd <- sqrt(mean((x - mean(x))^2))
cat("MLE for sd(biased):", mle_sd, "\n")

# Compare to unbiased estimator
unbiased_sd <- sd(x)
cat("Unbiased sd:", unbiased_sd, "\n")

# Profile likelihood for mean
mu_vals <- seq(3, 7, length.out = 100)
log_lik <- sapply (mu_vals, function(mu) {
  sum(dnorm(x, mean = mu, sd = 2, log = TRUE))
})

# Plot log - likelihood
plot(mu_vals, log_lik, type = "l",
     xlab = "mu", ylab = "Log - likelihood",
     main = "Profile Likelihood for Mean")
abline(v = mle_mean, col = "red", lty = 2)

#What is the role of the sapply command? Use the command line in R to find
#help typing: “?sapply”

##########################################################
#Exercise 4: Bootstrap
#Goal: Use the bootstrap to estimate confidence intervals
library(boot)

# Load data
data(iris)

# Bootstrap for correlation
boot_cor <- function (data, indices) {
  d <- data [indices, ]
  cor(d$ Sepal.Length, d$ Petal.Length)
}

# Run bootstrap
boot_out <- boot(iris, boot_cor, R = 1000)

# Results
boot_out
plot(boot_out)

# Confidence intervals
boot.ci(boot_out, type = "perc")
boot.ci(boot_out, type = "basic")
boot.ci(boot_out, type = "bca")

# Bootstrap for median
boot_median <- function(data, indices) {
  median(data[indices])
}

boot_out_median <- boot(iris$Sepal.Length, boot_median, R = 1000)
boot.ci(boot_out_median, type = "perc")

######################################################################
#Exercise 5: Comparison of Estimators
#Goal: Compare different estimators for Normal parameters
# Compare estimators for variance
n_sims <- 1000
n <- 30

estimates <- replicate(n_sims, {
  x <- rnorm(n, mean = 0, sd = 2)
  mle_var <- mean((x - mean (x))^2)
  unbiased_var <- var(x)
  c(mle_var, unbiased_var)
})

# Compare bias
mle_bias <- mean(estimates[1, ]) - 4
unbiased_bias <- mean(estimates[2, ]) - 4

cat("MLE bias:", mle_bias, "\n")
cat("Unbiased bias:", unbiased_bias, "\n")

# Compare MSE
mle_mse <- mean((estimates[1, ] - 4) ^2)
unbiased_mse <- mean((estimates[2, ] - 4) ^2)

cat("MLE MSE :", mle_mse, "\n")
cat ("Unbiased MSE :", unbiased_mse, "\n")

#########################################3
#exercício 6: challenge problems
#6.1. Demonstrate the CLT for the Chi-square distribution.
set.seed(123)

# Parameters
df <- 3          # Degrees of freedom
n <- 30          # Sample size
B <- 10000       # Number of samples

# Generate sample means
sample_means <- replicate(B, mean(rchisq(n, df = df)))

# Theoretical mean and standard error
mu <- df
se <- sqrt(2 * df / n)

# Histogram of sample means
hist(sample_means,
     probability = TRUE,
     breaks = 40,
     col = "lightblue",
     main = "CLT Demonstration: Chi-square(3)",
     xlab = "Sample Mean")

# Add the theoretical normal distribution
curve(dnorm(x, mean = mu, sd = se),
      add = TRUE,
      col = "red",
      lwd = 2)

# Add theoretical mean
abline(v = mu, col = "darkblue", lwd = 2, lty = 2)

legend("topright",
       legend = c("Sample means", "Normal approximation", "Population mean"),
       fill = c("lightblue", NA, NA),
       border = c("black", NA, NA),
       col = c(NA, "red", "darkblue"),
       lwd = c(NA, 2, 2),
       lty = c(NA, 1, 2))

#6.2. Compare bootstrap confidence intervals to asymptotic confidence interval
set.seed(123)

# Example data
data <- rnorm(50, mean = 10, sd = 2)


# Asymptotic confidence interval
n <- length(data)
mean_data <- mean(data)
se <- sd(data) / sqrt(n)

# 95% CI using the normal approximation
asymptotic_CI <- c(
  mean_data - 1.96 * se,
  mean_data + 1.96 * se
)


#Bootstrap confidence interval
B <- 10000

bootstrap_means <- replicate(
  B,
  mean(sample(data, size = n, replace = TRUE))
)

# Percentile bootstrap 95% CI
bootstrap_CI <- quantile(
  bootstrap_means,
  probs = c(0.025, 0.975)
)

# Display results
cat("Sample mean:", mean_data, "\n\n")

cat("Asymptotic 95% CI:",
    asymptotic_CI[1], "to", asymptotic_CI[2], "\n")

cat("Bootstrap 95% CI:",
    bootstrap_CI[1], "to", bootstrap_CI[2], "\n")

#6.3. Compute the MLE for a Gamma distribution using numerical optimization.
set.seed(123)

# Generate example data from Gamma(shape = 3, rate = 2)
x <- rgamma(100, shape = 3, rate = 2)

# Negative log-likelihood function
neg_log_likelihood <- function(par, x) {
  
  shape <- par[1]
  rate  <- par[2]
  
  # Parameters must be positive
  if (shape <= 0 || rate <= 0) {
    return(Inf)
  }
  
  # Negative log-likelihood
  -sum(dgamma(x,
              shape = shape,
              rate = rate,
              log = TRUE))
}

# Numerical optimization
mle <- optim(
  par = c(shape = 1, rate = 1),
  fn = neg_log_likelihood,
  x = x,
  method = "L-BFGS-B",
  lower = c(0.0001, 0.0001)
)

# MLE estimates
mle$par

