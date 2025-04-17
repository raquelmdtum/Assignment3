
#---- 3.1 plot the data -----

library(ggplot2)
library(tidyverse)

# Load data
data <- read_csv("E:/study/master4/time series/ass_class_3/box_data_60min.csv")

# Select the variables you want to plot
selected_data <- data %>%
  select(tdate, Ph, Tdelta, Gv)

# Convert to long format for ggplot
long_data <- pivot_longer(selected_data,
                          cols = c(Ph, Tdelta, Gv),
                          names_to = "Variable",
                          values_to = "Value")

# Plot the data and adjust x-axis tick spacing
ggplot(long_data, aes(x = tdate, y = Value, color = Variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Trend of Ph, Tdelta, and Gv Over Time",
       x = "Time",
       y = "Value") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +  # One tick per day, adjustable
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Tilt x-axis labels to avoid overlap

#---- 3.2 split the data ---

library(tidyverse)
library(lubridate)

# Just load the data — let read_csv handle tdate
data <- read_csv("E:/study/master4/time series/ass_class_3/box_data_60min.csv", show_col_types = FALSE)

# ✅ Don't touch tdate — it's already in POSIXct format

# Set the cutoff time (also in UTC to match)
cutoff_time <- ymd_hms("2013-02-06 01:00:00", tz = "UTC")

# Now split the data
train_data <- data %>% filter(tdate < cutoff_time)
test_data  <- data %>% filter(tdate >= cutoff_time)

# Check the result
cat("Training set rows:", nrow(train_data), "\n")
cat("Test set rows:", nrow(test_data), "\n")

# 3.3-----investigate the variables and their relationships

# Cross-correlation: Tdelta and Ph
ccf(train_data$Tdelta, train_data$Ph,
    lag.max = 50,
    main = "Cross-Correlation: Tdelta vs. Ph",
    na.action = na.pass)
# Cross-correlation: Gv and Ph
ccf(train_data$Gv, train_data$Ph,
    lag.max = 50,
    main = "Cross-Correlation: Gv vs. Ph",
    na.action = na.pass)
#scatter plot  
ggplot(train_data, aes(x = Tdelta, y = Ph)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # optional regression line
  labs(title = "Scatter Plot: Ph vs Tdelta",
       x = "Tdelta",
       y = "Ph") +
  theme_minimal()
ggplot(train_data, aes(x = Gv, y = Ph)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatter Plot: Ph vs Gv",
       x = "Gv",
       y = "Ph") +
  theme_minimal()
# auto-correlation # Plot autocorrelation of Ph
acf(train_data$Ph, 
    main = "Autocorrelation of Ph (Training Set)",
    lag.max = 50,    # You can adjust this number to see more or fewer lags
    na.action = na.pass)  # In case there are any NAs

# ---- 3.4 estimate the impulse response

# Fit model for impulse response from Tdelta to Ph
model_Tdelta <- lm(Ph ~ Tdelta.l0 + Tdelta.l1 + Tdelta.l2 + Tdelta.l3 + Tdelta.l4 +
                     Tdelta.l5 + Tdelta.l6 + Tdelta.l7 + Tdelta.l8 + Tdelta.l9 + Tdelta.l10,
                   data = train_data)

# Fit model for impulse response from Gv to Ph
model_Gv <- lm(Ph ~ Gv.l0 + Gv.l1 + Gv.l2 + Gv.l3 + Gv.l4 +
                 Gv.l5 + Gv.l6 + Gv.l7 + Gv.l8 + Gv.l9 + Gv.l10,
               data = train_data)
# Get coefficients (excluding intercept)
irf_Tdelta <- coef(model_Tdelta)[-1]
irf_Gv <- coef(model_Gv)[-1]

lags <- 0:10
# Combine into one data frame
df_irf <- data.frame(
  Lag = rep(lags, 2),
  IRF = c(irf_Tdelta, irf_Gv),
  Variable = rep(c("Tdelta", "Gv"), each = length(lags))
)

# Plot
# Data frame for Tdelta
df_irf_Tdelta <- data.frame(
  Lag = 0:10,
  Coefficient = as.numeric(irf_Tdelta)
)

# Plot
ggplot(df_irf_Tdelta, aes(x = Lag, y = Coefficient)) +
  geom_col(fill = "steelblue") +
  labs(title = "Impulse Response of Ph to Tdelta",
       x = "Lag",
       y = "Impulse Response Coefficient") +
  theme_minimal()
# Data frame for Gv
df_irf_Gv <- data.frame(
  Lag = 0:10,
  Coefficient = as.numeric(irf_Gv)
)

# Plot
ggplot(df_irf_Gv, aes(x = Lag, y = Coefficient)) +
  geom_col(fill = "indianred") +
  labs(title = "Impulse Response of Ph to Gv",
       x = "Lag",
       y = "Impulse Response Coefficient") +
  theme_minimal()

#----- 3.5 fit the linear model----

# 1. Fit the simple linear model without lags
model_simple <- lm(Ph ~ Tdelta + Gv, data = train_data)
summary(model_simple)  # view estimation results

# 2. One-step prediction on training data
train_data$Ph_pred <- predict(model_simple)

# 3. Residuals
train_data$residuals <- residuals(model_simple)

# Plot residuals
plot(train_data$residuals, type = "l", col = "blue", main = "Residuals", ylab = "Residual", xlab = "Time")

# 4. ACF of residuals
acf(train_data$residuals, main = "ACF of Residuals")

# 5. CCF of residuals vs Tdelta
ccf(train_data$residuals, train_data$Tdelta, main = "CCF: Residuals vs Tdelta")

# 6. CCF of residuals vs Gv
ccf(train_data$residuals, train_data$Gv, main = "CCF: Residuals vs Gv")


# ----- 3.6 ARX model ------
# === 1. Fit an ARX(1) model ===
model_arx <- lm(Ph ~ Ph.l1 + Tdelta + Gv, data = train_data)
summary(model_arx)  # View estimation results

# === 2. One-step ahead prediction (in-sample) ===
train_data$Ph_pred_arx <- predict(model_arx)

# === 3. Compute residuals ===
train_data$residuals_arx <- residuals(model_arx)

# === 4. Open a new window and plot actual vs predicted values ===
x11()  # On Windows; use quartz() on Mac, or X11() on Linux
plot(train_data$Ph, type = "l", col = "black", lwd = 2,
     main = "ARX Model: Actual vs Predicted",
     ylab = "Ph", xlab = "Time")
lines(train_data$Ph_pred_arx, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"),
       col = c("black", "red"), lty = 1)

# === 5. Open a new window and plot residuals ===
x11()
plot(train_data$residuals_arx, type = "l", col = "blue",
     main = "Residuals of ARX Model", ylab = "Residual", xlab = "Time")

# === 6. Open a new window and plot the ACF of residuals ===
x11()
acf(train_data$residuals_arx, main = "ACF of Residuals (ARX)")

# === 7. Print the R-squared value ===
cat("R-squared of ARX model:", summary(model_arx)$r.squared, "\n")



#------- 3.7 plot the AIC & BIC ------
# === Parameter setup ===
max_order <- 10  # Maximum number of ARX model orders to try
aic_values <- numeric(max_order)
bic_values <- numeric(max_order)

# === Loop through different model orders and fit ARX models ===
for (order in 1:max_order) {
  response <- "Ph"
  ar_terms <- paste0("Ph.l", 1:order)                      # Autoregressive (AR) lags of Ph
  tdelta_terms <- paste0("Tdelta.l", 0:(order - 1))        # Lags of Tdelta
  gv_terms <- paste0("Gv.l", 0:(order - 1))                # Lags of Gv
  predictors <- c(ar_terms, tdelta_terms, gv_terms)
  
  # Build the formula string and convert it to a formula object
  formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Fit the ARX model on training data (removing NA rows)
  data_used <- na.omit(train_data[, c(response, predictors)])
  model <- lm(formula, data = data_used)
  
  # Store AIC and BIC for the current model
  aic_values[order] <- AIC(model)
  bic_values[order] <- BIC(model)
}

# === Set a tighter y-axis range with some buffer ===
ymin <- min(c(aic_values, bic_values)) 
ymax <- max(c(aic_values, bic_values)) 

# === Plot AIC and BIC vs. model order ===
plot(1:max_order, aic_values, type = "b", col = "blue", pch = 16,
     ylim = c(ymin, ymax),
     xlab = "Model Order", ylab = "Criterion Value",
     main = "AIC and BIC vs. ARX Model Order")
lines(1:max_order, bic_values, type = "b", col = "red", pch = 17)
legend("topright", legend = c("AIC", "BIC"), col = c("blue", "red"), pch = c(16, 17))


# ---- 3.8 RMSE -------
# === Set the maximum ARX model order ===
max_order <- 10
rmse_values <- numeric(max_order)  # Vector to store RMSE for each order

# === Loop through model orders and fit ARX models ===
for (order in 1:max_order) {
  response <- "Ph"
  ar_terms <- paste0("Ph.l", 1:order)                     # AR lags
  tdelta_terms <- paste0("Tdelta.l", 0:(order - 1))       # Tdelta lags
  gv_terms <- paste0("Gv.l", 0:(order - 1))               # Gv lags
  predictors <- c(ar_terms, tdelta_terms, gv_terms)
  
  # Build the regression formula
  formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Fit the ARX model using training data
  train_clean <- na.omit(train_data[, c(response, predictors)])
  model <- lm(formula, data = train_clean)
  
  # Predict on test data
  test_clean <- na.omit(test_data[, c(response, predictors)])
  predictions <- predict(model, newdata = test_clean)
  
  # Compute RMSE on the test set
  errors <- predictions - test_clean$Ph
  rmse <- sqrt(mean(errors^2))
  rmse_values[order] <- rmse
}

# === Plot RMSE vs. model order ===
x11()  # On Windows; use quartz() on Mac or X11() on Linux
plot(1:max_order, rmse_values, type = "b", col = "darkgreen", pch = 16,
     xlab = "Model Order", ylab = "RMSE (Test Set)",
     main = "Test RMSE vs ARX Model Order")
grid()

# === Output the model order with the lowest RMSE ===
best_rmse_order <- which.min(rmse_values)
cat("Best model order based on RMSE:", best_rmse_order, "\n")



# ----- 3.9 multi-step prediction -----
# === Set your selected optimal model order (e.g., based on BIC) ===
best_order <- 4

# === Combine training and test datasets ===
full_data <- data
N <- nrow(full_data)

# Initialize the prediction result vector
Ph_pred_sim <- rep(NA, N)

# === Construct the ARX model formula ===
response <- "Ph"
ar_terms <- paste0("Ph.l", 1:best_order)
tdelta_terms <- paste0("Tdelta.l", 0:(best_order - 1))
gv_terms <- paste0("Gv.l", 0:(best_order - 1))
predictors <- c(ar_terms, tdelta_terms, gv_terms)
formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
formula <- as.formula(formula_str)

# === Fit the ARX model using only the training data ===
train_clean <- na.omit(train_data[, c(response, predictors)])
model <- lm(formula, data = train_clean)

# === Multi-step simulation ===
# Copy Ph column for simulation updates
Ph_sim <- full_data$Ph

# Begin simulation from the first time step after training set ends
for (t in (nrow(train_data) + 1):N) {
  # Skip early time steps if we don’t have enough lags
  if (t - best_order < 1) next
  
  # Extract AR lag values from previously simulated data
  ar_vals <- Ph_sim[(t - 1):(t - best_order)]
  tdelta_vals <- full_data[t, paste0("Tdelta.l", 0:(best_order - 1))]
  gv_vals <- full_data[t, paste0("Gv.l", 0:(best_order - 1))]
  
  # Create a one-row data frame for prediction
  new_data <- data.frame(as.list(c(
    setNames(ar_vals, paste0("Ph.l", 1:best_order)),
    setNames(as.numeric(tdelta_vals), paste0("Tdelta.l", 0:(best_order - 1))),
    setNames(as.numeric(gv_vals), paste0("Gv.l", 0:(best_order - 1)))
  )))
  
  # Predict Ph at time t
  Ph_pred_sim[t] <- predict(model, newdata = new_data)
  
  # Update simulated Ph for use in the next time step
  Ph_sim[t] <- Ph_pred_sim[t]
}

# === Plot actual vs predicted values ===
x11()
plot(full_data$Ph, type = "l", col = "black", lwd = 2,
     main = paste("Multi-step Prediction (ARX Order =", best_order, ")"),
     xlab = "Time", ylab = "Ph")
lines(Ph_pred_sim, col = "blue", lwd = 2, lty = 2)

legend("topright", legend = c("Actual", "Predicted"),
       col = c("black", "blue"), lty = c(1, 2), lwd = 2, box.lwd = 0)

      








