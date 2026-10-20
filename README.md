
# Time Series Analysis & Forecasting of Crude Oil Prices

A statistical time-series forecasting model built in **R** to analyze, decompose, and predict monthly crude oil price movements. This project applies classical decomposition techniques (multiplicative model) alongside **Auto-Regressive (AR(2))** modeling solved via the **Yule-Walker equations** to capture temporal dependencies.

---

## 🛠️ Tech Stack & Libraries
* **Language:** R
* **Core Packages:** `stats` (base R time-series functions), `trend` (for Mann-Kendall test)
* **Statistical Concepts:** Classical Decomposition, Moving Averages, Seasonal Indices, Autocorrelation Function (ACF), Yule-Walker Estimation, Stationarity Testing (Runs Test, Mann-Kendall Test).

---

## 📊 Dataset
* **Source:** Monthly crude oil price dataset spanning from 1986 onwards ($N = 408$ months / 34+ years).
* **Key Variables:** `oil_price` (monthly historical pricing).

---

## Prerequisites
install.packages("trend")

## Model Performance Comparison
Model     MAE         MAPE (%)
AR(1)   0.05526      5.8444%
AR(2)   0.05121      5.3720%

## Limitations & Future Scope:-
# Moving Average Boundary Loss: Moving average smoothing results in the loss of data points at the beginning and end of the series, restricting direct boundary forecasting.

# Model Assumption: Assumes a pure multiplicative framework; real-world commodities often follow mixed or dynamic economic structures.
# Future Scope: Implement higher-order ARIMA, SARIMA, or machine learning hybrids (such as LSTM networks) for capturing complex nonlinear crude oil price volatility.


## 📁 Project Structure
```text
📦 Oil-Price-Time-Series-Forecasting
 ┣ 📜 oil_price_dataset.csv     # Historical monthly oil price data
 ┣ 📜 analysis.R                # Full R script for decomposition and AR(2) modeling
 ┗ 📜 README.md                 # Project documentation


rm(list = ls())

# 1. Load Data
d = read.csv("oil_price_dataset.csv")
x = d$oil_price
data = ts(x, start = 1986, frequency = 12)
plot(data, type = "l", main = "Historical Monthly Oil Prices")

# 2. Moving Average Trend Extraction
ma = numeric(nrow(d) - 11)
for (i in 1:(nrow(d) - 11)) {
  ma[i] = mean(x[i:(i + 11)])
}

Moving_Average = numeric(length(ma) - 1)
for (i in 1:(length(ma) - 1)) {
  Moving_Average[i] = mean(ma[i:(i + 1)])
}

# 3. Multiplicative Detrending & Seasonal Indices
dtrnd = x[7:(nrow(d) - 6)] / Moving_Average
d2 = matrix(c(rep(0, 6), dtrnd, rep(0, 6)), byrow = T, ncol = 12)
USI = apply(d2, 2, sum) / (nrow(d) / 12)
ASI = USI * 1200 / sum(USI)

ASI_1 = c(ASI[c(7:12)], ASI[c(1:6)])
deseasonalize = as.vector(dtrnd) / rep(ASI_1, (nrow(d) / 12))

# 4. Stationarity & ACF
library(trend)
mat = matrix(x, ncol = 12, byrow = T)
mann = apply(mat, 1, sum)
mk.test(mann) # Mann-Kendall test for trend

acf(deseasonalize, main = "ACF Plot for Residuals")

# 5. AR(2) Estimation via Yule-Walker
x_t = deseasonalize[-1]
x_t1 = deseasonalize[-length(deseasonalize)]
# (Extend regression setup for lag 1 and lag 2 as outlined in project documentation)


