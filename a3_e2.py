#Exercise 2.1
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm

# Load the dataset
file_path = r"C:\Users\40731\Desktop\New folder\datasolar.csv"  # Make sure the file is in the same directory or provide the correct path
data = pd.read_csv(file_path)

# Given parameters
phi1 = -0.38
Phi1 = -0.94
mu = 5.72

# Transform power data: Xt = log(Yt) - mu
data["Xt"] = np.log(data["power"]) - mu

# Compute residuals using the model: Xt + 0.38 Xt-1 + 0.94 Xt-12 + (0.38 * 0.94) Xt-13 = εt
data["residual"] = np.nan  # Initialize residual column

for t in range(13, len(data)):
    data.loc[t, "residual"] = (
        data.loc[t, "Xt"] 
        + phi1 * data.loc[t-1, "Xt"]
        + Phi1 * data.loc[t-12, "Xt"]
        + (phi1 * Phi1) * data.loc[t-13, "Xt"]
    )

# Drop initial rows with NaN values due to lag computations
data_clean = data.dropna()

# Plot residuals
plt.figure(figsize=(10, 5))
plt.plot(data_clean["residual"], marker="o", linestyle="-", label="Residuals")
plt.axhline(0, color='r', linestyle="--", label="Zero Line")
plt.title("Residuals Over Time")
plt.xlabel("Time Index")
plt.ylabel("Residual")
plt.legend()
plt.show()

# Check normality and independence of residuals
sm.graphics.tsa.plot_acf(data_clean["residual"], lags=20, title="Autocorrelation of Residuals")
plt.show()

sm.qqplot(data_clean["residual"], line='s')
plt.title("Q-Q Plot of Residuals")
plt.show()

#Exercise 2.2
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm

# Load dataset
file_path = r"C:\Users\40731\Desktop\New folder\datasolar.csv"
data = pd.read_csv(file_path)

# Given parameters
mu = 5.72

# Transform power data: Xt = log(Yt) - mu
data["Xt"] = np.log(data["power"]) - mu

# Prepare dataset for regression
data["Xt_1"] = data["Xt"].shift(1)
data["Xt_12"] = data["Xt"].shift(12)
data["Xt_13"] = data["Xt"].shift(13)
data_clean = data.dropna()

# Define dependent and independent variables
y = data_clean["Xt"]
X_reg = data_clean[["Xt_1", "Xt_12", "Xt_13"]]

# Fit the OLS regression model
X_reg = sm.add_constant(X_reg)  # Adds intercept
model = sm.OLS(y, X_reg).fit()

# Extract estimated parameters
phi1_computed = -model.params["Xt_1"]
Phi1_computed = -model.params["Xt_12"]

# Forecasting for t = 36 and k = 1,...,12
t_last = 36
Xt_forecast = list(data_clean["Xt"].iloc[:t_last])  # Start with observed data

# Predict next 12 months
forecast_horizon = 12
for k in range(1, forecast_horizon + 1):
    Xt_next = -phi1_computed * Xt_forecast[-1] - Phi1_computed * Xt_forecast[-12] - (phi1_computed * Phi1_computed) * Xt_forecast[-13]
    Xt_forecast.append(Xt_next)

# Extract only the last 12 forecasted values
Yt_forecast = np.exp(np.array(Xt_forecast[-forecast_horizon:]) + mu)

# Ensure `forecast_months` has exactly 12 elements
forecast_months = np.arange(t_last + 1, t_last + 1 + forecast_horizon)

# Create forecast table
forecast_table = pd.DataFrame({
    "Month": forecast_months,
    "Predicted Power (MWh)": Yt_forecast
})

# Create forecast table
forecast_months = np.arange(t_last + 1, t_last + 1 + forecast_horizon)
forecast_table = pd.DataFrame({
    "Month": forecast_months,
    "Predicted Power (MWh)": Yt_forecast
})

print("\nForecasted Power for the Next 12 Months:\n")
print(forecast_table)

# Plot observed and predicted values
plt.figure(figsize=(10, 5))
plt.plot(data_clean.index[:t_last], np.exp(data_clean["Xt"][:t_last] + mu), label="Observed Power", marker="o")
plt.plot(forecast_months, Yt_forecast, label="Forecasted Power", marker="s", linestyle="--")
plt.xlabel("Time Index")
plt.ylabel("Power (MWh)")
plt.title("Observed vs Forecasted Power")
plt.legend()
plt.show()

#Exercise 2.3
# Exercise 2.3: Completely Refined Approach for Forecasting with Prediction Intervals

# Ensure t_last is the length of the cleaned data
t_last = len(data_clean)

# Calculate residuals variance (sigma^2) from AR(1) model
residuals = model.resid
sigma_squared = np.var(residuals)

# Set up the forecasting horizon (12 months)
forecast_horizon = 12

# Initialize lists to store forecasted values and bounds
forecasted_values = []
lower_bound = []
upper_bound = []

# Start the forecast from the last observed value
Xt_forecast = [data_clean["Xt"].iloc[t_last - 1]]  # Start with the last observed data point

# Iterate over the forecast horizon (12 months ahead)
for k in range(forecast_horizon):
    # Apply the AR(1) model for forecasting (Xt_next is influenced by the previous Xt)
    Xt_next = phi1_computed * Xt_forecast[-1]  # Update using the AR(1) model
    
    # Append the forecasted value
    Xt_forecast.append(Xt_next)
    
    # Transform to original scale (after forecasting)
    Yt_forecast = np.exp(Xt_next + mu)  # Convert back to the original power scale
    forecasted_values.append(Yt_forecast)
    
    # Calculate the standard deviation for the forecast at time t+k (based only on AR(1) model)
    forecast_stddev = np.sqrt(sigma_squared * (1 + (phi1_computed**2) * (1 - phi1_computed**(2 * (k + 1))) / (1 - phi1_computed**2)))
    
    # Calculate 95% prediction intervals (the bounds)
    lower = Yt_forecast - 1.96 * forecast_stddev
    upper = Yt_forecast + 1.96 * forecast_stddev
    lower_bound.append(lower)
    upper_bound.append(upper)

# Create forecast table
forecast_months = np.arange(t_last + 1, t_last + 1 + forecast_horizon)
forecast_table = pd.DataFrame({
    "Month": forecast_months,
    "Predicted Power (MWh)": forecasted_values,
    "Lower Bound (MWh)": lower_bound,
    "Upper Bound (MWh)": upper_bound
})

print("\nForecasted Power for the Next 12 Months with 95% Prediction Intervals:\n")
print(forecast_table)

# Plot observed and predicted values with prediction intervals
plt.figure(figsize=(10, 5))

# Plot the observed data (transform back to the original scale)
plt.plot(data_clean.index[:t_last], np.exp(data_clean["Xt"][:t_last] + mu), label="Observed Power", marker="o")

# Plot the forecasted values (mean)
plt.plot(forecast_months, forecasted_values, label="Forecasted Power", marker="s", linestyle="--")

# Plot the prediction intervals (max and min)
plt.fill_between(forecast_months, lower_bound, upper_bound, color='gray', alpha=0.3, label="95% Prediction Interval")

# Formatting the plot
plt.xlabel("Time Index")
plt.ylabel("Power (MWh)")
plt.title("Observed vs Forecasted Power with 95% Prediction Intervals")
plt.legend()
plt.show()
