# Install and load necessary packages
library(forecast)
library(ggplot2)
library(tidyr)
library(gridExtra)
set.seed(123) 

# ----------------------------------------

# ------- Exercise 1.1 ---------

# Define AR(2) parameters
phi1 <- -0.6
phi2 <- 0.5
phi <- c(-0.6, 0.5) 
sigma_e <- 1
n <- 200  # Number of observations
num_sim <- 5  # Number of realizations

# Simulate AR(2) process
generate_ar2 <- function(n, phi1, phi2, sigma_e) {
  e <- rnorm(n, mean = 0, sd = sigma_e)
  x <- numeric(n)
  x[1:2] <- rnorm(2)  # Initial values
  
  for (t in 3:n) {
    x[t] <- - phi1 * x[t-1] - phi2 * x[t-2] + e[t]
  }
  return(x)
}

simulations <- replicate(num_sim, generate_ar2(n, phi1, phi2, sigma_e))

# Convert to dataframe for ggplot using tidyr
sim_df <- data.frame(time = 1:n, as.data.frame(simulations))
sim_df <- pivot_longer(sim_df, cols = -time, names_to = "simulation", values_to = "value")

# Plot the simulations
plot1 <- ggplot(sim_df, aes(x = time, y = value, color = simulation)) +
  geom_line() +
  labs(title = "Simulations of AR(2) Process", x = "Time", y = "X_t") +
  theme_minimal()

# Save the first plot
ggsave("Figures/1.1_plot.pdf", plot = plot1, width = 10, height = 5, dpi = 300)

# ------- Exercise 1.2 ---------

# Number of lags
lag_max <- 30

# Step 1: Compute empirical ACFs for each simulation
acf_list <- lapply(1:num_sim, function(i) {
  acf(simulations[, i], lag.max = lag_max, plot = FALSE)
})

# Step 2: Convert each ACF result into a dataframe
empirical_acf_df <- do.call(rbind, lapply(1:num_sim, function(i) {
  data.frame(
    lag = acf_list[[i]]$lag[, 1, 1],
    acf = acf_list[[i]]$acf[, 1, 1],
    simulation = paste0("Sim ", i)
  )
}))

# Step 3: Get the theoretical ACF using ARMAacf
theoretical_acf <- ARMAacf(ar = phi, ma = numeric(0), lag.max = lag_max)
theoretical_df <- data.frame(
  lag = 0:lag_max,
  acf = theoretical_acf,
  simulation = "Theoretical"
)

# Step 4: Combine data
acf_combined_df <- rbind(empirical_acf_df, theoretical_df)

# Step 5: Plot
plot_acf_combined <- ggplot(acf_combined_df, aes(x = lag, y = acf, color = simulation)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Empirical ACFs of 5 Simulations vs Theoretical ACF",
       x = "Lag", y = "ACF") +
  theme_minimal()

ggsave("Figures/1.2_empirical_vs_theoretical_acf.pdf", plot = plot_acf_combined, width = 10, height = 5, dpi = 300)

# ------- Exercise 1.3 ---------

# Define AR(2) parameters
phi1 <- -0.6
phi2 <- -0.3
phi <- c(-0.6, -0.3) 
sigma_e <- 1
n <- 200  # Number of observations
num_sim <- 5  # Number of realizations

# Simulate AR(2) process
generate_ar2 <- function(n, phi1, phi2, sigma_e) {
  e <- rnorm(n, mean = 0, sd = sigma_e)
  x <- numeric(n)
  x[1:2] <- rnorm(2)  # Initial values
  
  for (t in 3:n) {
    x[t] <- - phi1 * x[t-1] - phi2 * x[t-2] + e[t]
  }
  return(x)
}

simulations <- replicate(num_sim, generate_ar2(n, phi1, phi2, sigma_e))

# Convert to dataframe for ggplot using tidyr
sim_df <- data.frame(time = 1:n, as.data.frame(simulations))
sim_df <- pivot_longer(sim_df, cols = -time, names_to = "simulation", values_to = "value")

# Plot the simulations
plot1 <- ggplot(sim_df, aes(x = time, y = value, color = simulation)) +
  geom_line() +
  labs(title = "Simulations of AR(2) Process", x = "Time", y = "X_t") +
  theme_minimal()

# Save the first plot
ggsave("Figures/1.3_plot.pdf", plot = plot1, width = 10, height = 5, dpi = 300)

# ------- Exercise 1.4 ---------

# Define AR(2) parameters
phi1 <- 0.6
phi2 <- -0.3
phi <- c(0.6, -0.3) 
sigma_e <- 1
n <- 200  # Number of observations
num_sim <- 5  # Number of realizations

# Simulate AR(2) process
generate_ar2 <- function(n, phi1, phi2, sigma_e) {
  e <- rnorm(n, mean = 0, sd = sigma_e)
  x <- numeric(n)
  x[1:2] <- rnorm(2)  # Initial values
  
  for (t in 3:n) {
    x[t] <- - phi1 * x[t-1] - phi2 * x[t-2] + e[t]
  }
  return(x)
}

simulations <- replicate(num_sim, generate_ar2(n, phi1, phi2, sigma_e))

# Convert to dataframe for ggplot using tidyr
sim_df <- data.frame(time = 1:n, as.data.frame(simulations))
sim_df <- pivot_longer(sim_df, cols = -time, names_to = "simulation", values_to = "value")

# Plot the simulations
plot1 <- ggplot(sim_df, aes(x = time, y = value, color = simulation)) +
  geom_line() +
  labs(title = "Simulations of AR(2) Process", x = "Time", y = "X_t") +
  theme_minimal()

# Save the first plot
ggsave("Figures/1.4_plot.pdf", plot = plot1, width = 10, height = 5, dpi = 300)

# ------- Exercise 1.5 ---------

# Define AR(2) parameters
phi1 <- -0.7
phi2 <- -0.3
phi <- c(-0.7, -0.3) 
sigma_e <- 1
n <- 200  # Number of observations
num_sim <- 5  # Number of realizations

# Simulate AR(2) process
generate_ar2 <- function(n, phi1, phi2, sigma_e) {
  e <- rnorm(n, mean = 0, sd = sigma_e)
  x <- numeric(n)
  x[1:2] <- rnorm(2)  # Initial values
  
  for (t in 3:n) {
    x[t] <- - phi1 * x[t-1] - phi2 * x[t-2] + e[t]
  }
  return(x)
}

simulations <- replicate(num_sim, generate_ar2(n, phi1, phi2, sigma_e))

# Convert to dataframe for ggplot using tidyr
sim_df <- data.frame(time = 1:n, as.data.frame(simulations))
sim_df <- pivot_longer(sim_df, cols = -time, names_to = "simulation", values_to = "value")

# Plot the simulations
plot1 <- ggplot(sim_df, aes(x = time, y = value, color = simulation)) +
  geom_line() +
  labs(title = "Simulations of AR(2) Process", x = "Time", y = "X_t") +
  theme_minimal()

# Save the first plot
ggsave("Figures/1.5_plot.pdf", plot = plot1, width = 10, height = 5, dpi = 300)

# ------- Exercise 1.6 ---------

# Define AR(2) parameters
phi1 <- -0.75
phi2 <- -0.3
phi <- c(-0.7, -0.3) 
sigma_e <- 1
n <- 200  # Number of observations
num_sim <- 5  # Number of realizations

# Simulate AR(2) process
generate_ar2 <- function(n, phi1, phi2, sigma_e) {
  e <- rnorm(n, mean = 0, sd = sigma_e)
  x <- numeric(n)
  x[1:2] <- rnorm(2)  # Initial values
  
  for (t in 3:n) {
    x[t] <- - phi1 * x[t-1] - phi2 * x[t-2] + e[t]
  }
  return(x)
}

simulations <- replicate(num_sim, generate_ar2(n, phi1, phi2, sigma_e))

# Convert to dataframe for ggplot using tidyr
sim_df <- data.frame(time = 1:n, as.data.frame(simulations))
sim_df <- pivot_longer(sim_df, cols = -time, names_to = "simulation", values_to = "value")

# Plot the simulations
plot1 <- ggplot(sim_df, aes(x = time, y = value, color = simulation)) +
  geom_line() +
  labs(title = "Simulations of AR(2) Process", x = "Time", y = "X_t") +
  theme_minimal()

# Save the first plot
ggsave("Figures/1.6_plot.pdf", plot = plot1, width = 10, height = 5, dpi = 300)

# ----------------------------------------