# th2Forecast

`th2Forecast` is an R package designed for automated time series forecasting and machine learning model evaluation. It provides an integrated framework that streamlines the complete forecasting pipeline, from data preparation to final prediction visualization.

## Main Features

- **Automated Preprocessing**: Includes robust functions for cleaning time series data, handling missing values, detecting anomalies, and identifying level shifts.
- **Advanced Feature Engineering**: Specialized modules for generating lag features and integrating exogenous data (such as holidays or weather) to enhance machine learning model performance.
- **Diverse Model Support**: Predict using a wide array of engines, including:
  - ARIMA
  - Prophet
  - MARS
  - Linear Regression
  - Random Forest
  - XGBoost
- **Model Tuning & Resampling**: Optimize hyper-parameters and validate models using time-series cross-validation.
- **Scalability**: Support for distributed processing via Spark for large-scale forecasting tasks.
- **Interactive UI**: Comes with a built-in Shiny module for interactive data upload, model configuration, and performance visualization.

## Workflow

1.  **Data Ingestion**: Upload your time series dataset.
2.  **Preprocessing**: Clean data and resolve anomalies.
3.  **Feature Engineering**: Generate predictive features optimized for ML algorithms.
4.  **Modeling**: Select algorithms, tune parameters, and train models.
5.  **Forecasting**: Generate forecasts and visualize predictive performance.

## Installation

You can install `th2Forecast` from GitHub:

```R
# install.packages("devtools")
devtools::install_github("apowerb/th2forecast")
```

## Getting Started

To launch the interactive Shiny interface:

```R
library(th2forecast)
run_app()
```

## API Access
`th2Forecast` includes an integrated Plumber API for remote forecasting tasks. For details on how to interact with the API, please refer to the documentation or the endpoint definitions in your deployment.

## License
GLP-3
