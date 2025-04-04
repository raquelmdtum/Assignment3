# Install and load necessary packages
# install.packages(c("forecast", "ggplot2"))
library(forecast)
library(ggplot2)
library(tidyr)

# Define AR(2) parameters
phi1 <- -0.6
phi2 <- 0.5
sigma_e <- 1
n <- 200  # Number of observations
num_sim <- 5  # Number of realizations

# Simulate AR(2) process
generate_ar2 <- function(n, phi1, phi2, sigma_e) {
  e <- rnorm(n, mean = 0, sd = sigma_e)
  x <- numeric(n)
  x[1:2] <- rnorm(2)  # Initial values
  
  for (t in 3:n) {
    x[t] <- -phi1 * x[t-1] - phi2 * x[t-2] + e[t]
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

# Compute and plot ACF
acf_list <- lapply(1:num_sim, function(i) acf(simulations[,i], plot = FALSE, lag.max = 30))

# Prepare ACF data for plotting
acf_df <- data.frame(lag = acf_list[[1]]$lag, acf = acf_list[[1]]$acf)

# Plot empirical ACF
plot2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_line(color = "blue") +
  labs(title = "Empirical ACF of Simulated AR(2) Process", x = "Lag", y = "ACF") +
  theme_minimal()

# Print both plots
print(plot1)
#print(plot2)
