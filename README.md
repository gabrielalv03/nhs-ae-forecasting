# nhs-ae-forecasting
# 📊 NHS A&E Forecasting with Machine Learning

This project uses machine learning to forecast pressure in NHS Accident & Emergency (A&E) departments across England. It combines open NHS data with regional socioeconomic indicators to build explainable models that predict surges in patient attendances and breaches of the 4-hour wait target.

---

## 📁 Project Structure

<pre> ## 🗂️ Project Structure ``` . ├── data/ # Raw and cleaned input data │ ├── socioeconomic_data/ # IMD, population, GP access, etc. │ └── links.md # External links to raw data sources ├── scripts/ # R scripts for data prep, modelling, and visualisation ├── README.md # Project overview ├── LICENSE # MIT License ``` </pre>

---

## 📌 Goals

- Forecast A&E attendances and % patients waiting >4h.
- Compare multiple forecasting models: Linear Regression, Random Forest, XGBoost, Prophet, and LSTM.
- Incorporate regional factors: deprivation, weather, GP access, etc.
- Generate risk heatmaps to inform NHS resource allocation.

---

## 📊 Data Sources

The data sources are all in the "data" directory

---

## 📈 Models and Evaluation

The following models were trained on 10 years of monthly data and evaluated on 2025 test data:

| Model         | Type           | Use Case                 |
|---------------|----------------|--------------------------|
| Linear Model  | Baseline       | Simple trend detection   |
| Random Forest | Nonlinear      | Feature importance       |
| XGBoost       | Ensemble       | Highest accuracy (A)     |
| Prophet       | Time series    | Captures seasonality     |

---

## 📌 Key Features

- ✅ Custom feature engineering (e.g. weighted IMD by Trust)
- ✅ One-hot encoding for seasonal effects
- ✅ Test-train split and RMSE/MAE evaluation
- ✅ Forecast visualisations and error heatmaps
- ✅ Reproducible R scripts

---

## 🚀 How to Reproduce

1. Clone this repo:
   ```bash
   git clone https://github.com/gabrielalv03/nhs-ae-forecasting.git
Open scripts/ and run in order
