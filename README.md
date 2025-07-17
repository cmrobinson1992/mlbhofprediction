# MLB Hall of Fame Prediction with Interactive Dashboard

This project provides an interactive dashboard to predict and analyze the Hall of Fame candidacy of current and former Major League Baseball (MLB) players. The tool uses player statistics from the Lahman database and a machine learning model trained on historical data to estimate the likelihood of induction into the National Baseball Hall of Fame.

## Project Overview

The dashboard allows users to:
- View career and age-based statistics for MLB players.
- Predict Hall of Fame induction probability using a trained model.
- Compare current players with historical players at similar career stages.
- Interpret model predictions with tools like SHAP and feature contributions.

## Machine Learning Model

- The predictive model is a **Gradient Boosted Machine (GBM)** trained on historical player stats.
- Input features include peak career values, career totals, rate stats, and award shares.
- The model is stored in `FinalModel.rds` and is trained using a cross-validated grid search for hyperparameter tuning.

## Project Structure

| File | Description |
|------|-------------|
| `data_clean.rds` | Cleaned and feature-engineered dataset used for model training |
| `train_data.rds` | Training dataset for model fitting |
| `test_data.rds` | Testing dataset used for evaluation |
| `FinalModel.rds` | Trained GBM model used for prediction |
| `app.R` or `dashboard.R` | (Not yet included) Main Shiny dashboard script |
| `README.md` | Project description and usage guide |

## Setup & Usage

### Requirements

Make sure you have the following R packages installed:

```r
install.packages(c("shiny", "tidyverse", "gbm", "iml", "pROC", "caret", "randomForest", "e1071", "class", "readr"))
