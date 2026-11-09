#Lab 5 Inverse Transform Method
# Set seed
set.seed(123)

# Inverse Transform for Exponential distribution
rexp_custom <- function(n, rate = 1) {
  u <- runif(n)
  x <- -log(1 - u) / rate
  return(x)
}

# Test the generator
n <- 10000
rate <- 2

custom_exp <- rexp_custom(n, rate)
builtin_exp <- rexp(n, rate)

# Compare
par(mfrow = c(1, 2))

hist(custom_exp, probability = TRUE, breaks = 50,
     main = "Custom Exponential", col = "lightblue")
curve(dexp(x, rate = rate), add = TRUE, col = "red", lwd = 2)

hist(builtin_exp, probability = TRUE, breaks = 50,
     main = "Built-in Exponential", col = "lightblue")
curve(dexp(x, rate = rate), add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Q-Q plot
qqplot(builtin_exp, custom_exp,
       main = "Q-Q Plot: Custom vs Built-in")
abline(0, 1, col = "red", lwd = 2)

# Kolmogorov-Smirnov test
ks.test(custom_exp, "pexp", rate = rate)

#EXERCISES
# Increase sample size and regenerate the custom exponential sample
n <- 50000
custom_exp <- rexp_custom(n, rate)
builtin_exp <- rexp(n, rate)

# Compare
par(mfrow = c(1, 2))

hist(custom_exp, probability = TRUE, breaks = 50,
     main = "Custom Exponential", col = "lightblue")
curve(dexp(x, rate = rate), add = TRUE, col = "red", lwd = 2)

hist(builtin_exp, probability = TRUE, breaks = 50,
     main = "Built-in Exponential", col = "lightblue")
curve(dexp(x, rate = rate), add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Q-Q plot
qqplot(builtin_exp, custom_exp,
       main = "Q-Q Plot: Custom vs Built-in")
abline(0, 1, col = "red", lwd = 2)

# Kolmogorov-Smirnov test
ks.test(custom_exp, "pexp", rate = rate)

#Exercise 2: More Continuous Distributions
# Inverse Transform for Laplace distribution
rlaplace_custom <- function(n, mu = 0, b = 1) {
  u <- runif(n)
  x <- mu - b * sign(u - 0.5) * log(1 - 2 * abs(u - 0.5))
  return(x)
}

# Test Laplace
n <- 10000
mu <- 2
b <- 1.5

custom_laplace <- rlaplace_custom(n, mu, b)

# Compare to built-in (if available)
# install.packages("extraDistr")
# library(extraDistr)
# builtin_laplace <- rlaplace(n, mu, b)

hist(custom_laplace, probability = TRUE, breaks = 50,
     main = "Laplace Distribution", col = "lightblue")

# Inverse Transform for Weibull distribution
rweibull_custom <- function(n, shape, scale = 1) {
  u <- runif(n)
  x <- scale * (-log(1 - u))^(1 / shape)
  return(x)
}

# Test Weibull
shape <- 2
scale <- 1

custom_weibull <- rweibull_custom(n, shape, scale)

hist(custom_weibull, probability = TRUE, breaks = 50,
     main = "Weibull Distribution", col = "lightblue")

# Compare with theoretical distributions
curve(dweibull(x, shape, scale), add = TRUE, col = "red", lwd = 2)

#PROBLEM
# Compare Laplace and Weibull distributions for different sample sizes
sample_sizes <- c(100, 1000, 10000)

mu <- 2
b <- 1.5
shape <- 2
scale <- 1

par(mfrow = c(3, 2))

for (n in sample_sizes) {
  
  # Laplace distribution
  custom_laplace <- rlaplace_custom(n, mu, b)
  
  hist(custom_laplace, probability = TRUE, breaks = 50,
       main = paste("Laplace, n =", n), col = "lightblue",
       xlab = "x")
  
  curve(exp(-abs(x - mu) / b) / (2 * b),
        add = TRUE, col = "red", lwd = 2)
  
  # Weibull distribution
  custom_weibull <- rweibull_custom(n, shape, scale)
  
  hist(custom_weibull, probability = TRUE, breaks = 50,
       main = paste("Weibull, n =", n), col = "lightblue",
       xlab = "x")
  
  curve(dweibull(x, shape, scale),
        add = TRUE, col = "red", lwd = 2)
}

par(mfrow = c(1, 1))

#Exercise 3: Discrete Inversion
# Inverse Transform for Bernoulli
rbernoulli_custom <- function(n, p = 0.5) {
  u <- runif(n)
  x <- as.numeric(u <= p)
  return(x)
}

# Inverse Transform for Geometric
rgeom_custom <- function(n, p = 0.5) {
  u <- runif(n)
  x <- floor(log(1 - u) / log(1 - p))
  return(x)
}

# Test Geometric
n <- 10000
p <- 0.3

custom_geom <- rgeom_custom(n, p)
builtin_geom <- rgeom(n, p)

par(mfrow = c(1, 2))

hist(custom_geom, probability = TRUE, breaks = 20,
     main = "Custom Geometric", col = "lightblue")
points(0:20, dgeom(0:20, p), type = "h", col = "red", lwd = 2)

hist(builtin_geom, probability = TRUE, breaks = 20,
     main = "Built-in Geometric", col = "lightblue")
points(0:20, dgeom(0:20, p), type = "h", col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Inverse Transform for Poisson (simple version)
rpois_custom <- function(n, lambda) {
  u <- runif(n)
  x <- numeric(n)
  
  for (i in 1:n) {
    cum_prob <- 0
    k <- 0
    
    while (cum_prob < u[i]) {
      cum_prob <- cum_prob + dpois(k, lambda)
      k <- k + 1
    }
    
    x[i] <- k - 1
  }
  
  return(x)
}

# Test Poisson
n <- 1000
lambda <- 3

custom_pois <- rpois_custom(n, lambda)

# Compare
table(custom_pois)
table(rpois(n, lambda))

#Exercise: implement a graphic comparison between the custom and builtin 
#generators of Poisson distributed random variables
n <- 10000
lambda <- 3

custom_pois <- rpois_custom(n, lambda)
builtin_pois <- rpois(n, lambda)

# Graphic comparison
par(mfrow = c(1, 2))

hist(custom_pois, probability = TRUE, breaks = seq(-0.5, max(custom_pois) + 0.5, 1),
     main = "Custom Poisson", col = "lightblue",
     xlab = "Values", ylim = c(0, 0.3))

points(0:max(custom_pois), dpois(0:max(custom_pois), lambda),
       type = "h", col = "red", lwd = 2)

hist(builtin_pois, probability = TRUE, breaks = seq(-0.5, max(builtin_pois) + 0.5, 1),
     main = "Built-in Poisson", col = "lightblue",
     xlab = "Values", ylim = c(0, 0.3))

points(0:max(builtin_pois), dpois(0:max(builtin_pois), lambda),
       type = "h", col = "red", lwd = 2)

par(mfrow = c(1, 1))

#Exercise 4: Validation
validate_generator <- function(custom_sample, theoretical_cdf, ...) {
  
  # Histogram
  hist(custom_sample, probability = TRUE, breaks = 30,
       main = "Validation")
  
  # Kolmogorov-Smirnov test
  ks_result <- ks.test(custom_sample, theoretical_cdf, ...)
  print(ks_result)
  
  # Chi-square test (for discrete)
  return(ks_result$p.value)
}

# Test Exponential
n <- 10000
rate <- 2

custom <- rexp_custom(n, rate)
p_val <- validate_generator(custom, "pexp", rate = rate)

cat("KS p-value:", p_val, "\n")

# Test Geometric
custom_geom <- rgeom_custom(n, p = 0.3)

ks.test(custom_geom, "pgeom", prob = 0.3)

#Exercise: use your validate generator function to investigate additional distributions
n <- 10000
mu <- 2
b <- 1.5

custom_laplace <- rlaplace_custom(n, mu, b)

# Laplace theoretical CDF
plaplace_custom <- function(x, mu, b) {
  ifelse(x < mu,
         0.5 * exp((x - mu) / b),
         1 - 0.5 * exp(-(x - mu) / b))
}

p_val_laplace <- validate_generator(
  custom_laplace, plaplace_custom,
  mu = mu, b = b
)

cat("Laplace KS p-value:", p_val_laplace, "\n")


# Test Weibull
shape <- 2
scale <- 1

custom_weibull <- rweibull_custom(n, shape, scale)

p_val_weibull <- validate_generator(
  custom_weibull, "pweibull",
  shape = shape, scale = scale
)

cat("Weibull KS p-value:", p_val_weibull, "\n")

#Exercise: include the theoretical curve in the graph of your validate generator function
validate_generator <- function(custom_sample, theoretical_cdf,
                               theoretical_pdf, ...) {
  
  # Histogram
  hist(custom_sample, probability = TRUE, breaks = 30,
       main = "Validation", col = "lightblue",
       xlab = "Values")
  
  # Theoretical density curve
  curve(do.call(theoretical_pdf,
                c(list(x = x), list(...))),
        add = TRUE, col = "red", lwd = 2)
  
  # Kolmogorov-Smirnov test
  ks_result <- ks.test(custom_sample, theoretical_cdf, ...)
  print(ks_result)
  
  return(ks_result$p.value)
}

# Test Exponential
n <- 10000
rate <- 2

custom <- rexp_custom(n, rate)

p_val <- validate_generator(
  custom, "pexp", "dexp", rate = rate
)

cat("Exponential KS p-value:", p_val, "\n")


# Test Weibull
shape <- 2
scale <- 1

custom_weibull <- rweibull_custom(n, shape, scale)

p_val_weibull <- validate_generator(
  custom_weibull, "pweibull", "dweibull",
  shape = shape, scale = scale
)

cat("Weibull KS p-value:", p_val_weibull, "\n")

#Exercise 5: Efficiency Comparison
library(microbenchmark)

# Compare speed
n <- 100000

microbenchmark(
  custom = rexp_custom(n, rate = 2),
  builtin = rexp(n, rate = 2),
  times = 10
)

# Compare discrete
microbenchmark(
  custom = rpois_custom(n, lambda = 3),
  builtin = rpois(n, lambda = 3),
  times = 10
)

# Exercise: perform a comparison between the custom and built-in generators 
# of random variables considering both the computational efficiency 
# and the quality of the generated histograms

#library(microbenchmark)

n <- 100000

# Parameters
rate <- 2
p <- 0.3
lambda <- 3

# Generate samples
custom_exp <- rexp_custom(n, rate)
builtin_exp <- rexp(n, rate)

custom_geom <- rgeom_custom(n, p)
builtin_geom <- rgeom(n, p)

custom_pois <- rpois_custom(n, lambda)
builtin_pois <- rpois(n, lambda)

# Compare computational efficiency
microbenchmark(
  custom_exp = rexp_custom(n, rate),
  builtin_exp = rexp(n, rate),
  times = 10
)

microbenchmark(
  custom_geom = rgeom_custom(n, p),
  builtin_geom = rgeom(n, p),
  times = 10
)

microbenchmark(
  custom_pois = rpois_custom(n, lambda),
  builtin_pois = rpois(n, lambda),
  times = 10
)

# Compare histogram quality
par(mfrow = c(3, 2))

# Exponential
hist(custom_exp, probability = TRUE, breaks = 50,
     main = "Custom Exponential", col = "lightblue")
curve(dexp(x, rate), add = TRUE, col = "red", lwd = 2)

hist(builtin_exp, probability = TRUE, breaks = 50,
     main = "Built-in Exponential", col = "lightblue")
curve(dexp(x, rate), add = TRUE, col = "red", lwd = 2)

# Geometric
hist(custom_geom, probability = TRUE,
     breaks = seq(-0.5, max(custom_geom) + 0.5, 1),
     main = "Custom Geometric", col = "lightblue")
points(0:max(custom_geom), dgeom(0:max(custom_geom), p),
       type = "h", col = "red", lwd = 2)

hist(builtin_geom, probability = TRUE,
     breaks = seq(-0.5, max(builtin_geom) + 0.5, 1),
     main = "Built-in Geometric", col = "lightblue")
points(0:max(builtin_geom), dgeom(0:max(builtin_geom), p),
       type = "h", col = "red", lwd = 2)

# Poisson
hist(custom_pois, probability = TRUE,
     breaks = seq(-0.5, max(custom_pois) + 0.5, 1),
     main = "Custom Poisson", col = "lightblue")
points(0:max(custom_pois), dpois(0:max(custom_pois), lambda),
       type = "h", col = "red", lwd = 2)

hist(builtin_pois, probability = TRUE,
     breaks = seq(-0.5, max(builtin_pois) + 0.5, 1),
     main = "Built-in Poisson", col = "lightblue")
points(0:max(builtin_pois), dpois(0:max(builtin_pois), lambda),
       type = "h", col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Challenge:
#   1. Implement the Inverse Transform Method for the Beta
#distribution (use the incomplete beta function)
rbeta_custom <- function(n, shape1, shape2) {
  u <- runif(n)
  x <- qbeta(u, shape1, shape2)
  return(x)
}

# Test Beta
n <- 10000
shape1 <- 2
shape2 <- 5

custom_beta <- rbeta_custom(n, shape1, shape2)
builtin_beta <- rbeta(n, shape1, shape2)

# Compare
par(mfrow = c(1, 2))

hist(custom_beta, probability = TRUE, breaks = 50,
     main = "Custom Beta", col = "lightblue")
curve(dbeta(x, shape1, shape2), add = TRUE,
      col = "red", lwd = 2)

hist(builtin_beta, probability = TRUE, breaks = 50,
     main = "Built-in Beta", col = "lightblue")
curve(dbeta(x, shape1, shape2), add = TRUE,
      col = "red", lwd = 2)

par(mfrow = c(1, 1))


#2. Create a generator for a mixture distribution using inversion
rmix_custom <- function(n, p = 0.5) {
  u <- runif(n)
  x <- numeric(n)
  
  # Mixture of two exponential distributions
  rate1 <- 2
  rate2 <- 5
  
  for (i in 1:n) {
    if (u[i] <= p) {
      x[i] <- qexp(u[i] / p, rate = rate1)
    } else {
      x[i] <- qexp((u[i] - p) / (1 - p), rate = rate2)
    }
  }
  
  return(x)
}

# Test Mixture
n <- 10000
p <- 0.5

custom_mix <- rmix_custom(n, p)

# Compare with theoretical density
hist(custom_mix, probability = TRUE, breaks = 50,
     main = "Mixture Distribution", col = "lightblue")

curve(p * dexp(x, rate = 2) +
        (1 - p) * dexp(x, rate = 5),
      add = TRUE, col = "red", lwd = 2)

# 3. Compare the efficiency of the simple Poisson generator
# to a more efficient algorithm

# Efficient Poisson generator (Knuth's algorithm)
rpois_knuth <- function(n, lambda) {
  x <- numeric(n)
  L <- exp(-lambda)
  
  for (i in 1:n) {
    k <- 0
    p <- 1
    
    repeat {
      k <- k + 1
      p <- p * runif(1)
      
      if (p <= L) break
    }
    
    x[i] <- k - 1
  }
  
  return(x)
}

# Compare efficiency
library(microbenchmark)

n <- 10000
lambda <- 3

microbenchmark(
  simple = rpois_custom(n, lambda),
  knuth = rpois_knuth(n, lambda),
  builtin = rpois(n, lambda),
  times = 10
)

# Compare histograms
custom_simple <- rpois_custom(n, lambda)
custom_knuth <- rpois_knuth(n, lambda)
builtin_pois <- rpois(n, lambda)

par(mfrow = c(1, 3))

hist(custom_simple, probability = TRUE, breaks = 20,
     main = "Simple Poisson", col = "lightblue")
points(0:max(custom_simple), dpois(0:max(custom_simple), lambda),
       type = "h", col = "red", lwd = 2)

hist(custom_knuth, probability = TRUE, breaks = 20,
     main = "Knuth Poisson", col = "lightblue")
points(0:max(custom_knuth), dpois(0:max(custom_knuth), lambda),
       type = "h", col = "red", lwd = 2)

hist(builtin_pois, probability = TRUE, breaks = 20,
     main = "Built-in Poisson", col = "lightblue")
points(0:max(builtin_pois), dpois(0:max(builtin_pois), lambda),
       type = "h", col = "red", lwd = 2)

par(mfrow = c(1, 1))