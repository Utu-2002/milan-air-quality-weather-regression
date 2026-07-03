# Weather Factors Impacting Air Quality in Milan

Regression and clustering analysis of how weather conditions influence air quality pollutants in Milan.

## Overview

This project analyzes how meteorological conditions — temperature, wind speed, precipitation, and humidity — influence four key air quality pollutants: **NO2, O3, PM2.5, and PM10**. The analysis progresses from simple linear regression through multiple and polynomial regression, followed by unsupervised clustering to uncover seasonal pollution patterns.

## Methodology

**1. Simple Regression**
Each pollutant regressed individually against wind speed to establish a baseline relationship.

**2. Multiple Regression**
Each pollutant modeled as a function of temperature, wind, and precipitation jointly, with an influence-strength comparison chart showing which weather factor drives each pollutant most.

**3. Polynomial Regression**
Extended models incorporating quadratic terms (temperature², wind², humidity²) and interaction effects (temperature×humidity, temperature×wind, wind×precipitation, wind×humidity) to capture non-linear relationships.

**4. Model Comparison**
Simple, multiple, and polynomial models compared side-by-side via actual-vs-predicted plots for each pollutant.

**5. K-Means Clustering**
Unsupervised clustering (k=3) across all pollutants and weather variables jointly, visualized as pollutant-vs-wind scatterplots colored by cluster assignment.

**6. Seasonal Analysis**
Two complementary views of seasonality:
- **Calendar-based:** pollutant vs. wind speed, colored by actual meteorological season (Winter/Spring/Summer/Autumn)
- **Data-driven:** temperature-only k-means (k=4) re-mapped to season labels by cluster temperature ranking, applied to O3 and to average pollution (NO2, PM2.5, PM10), to test whether pollution patterns align with data-driven rather than calendar-defined seasons

## Tech Stack

R · ggplot2 · dplyr · tidyr · readxl · stats (lm, kmeans)

## Files

- `Vantini_project.R` — full analysis script (regression, polynomial modeling, clustering, visualizations)

## Course Context

Project completed as part of coursework at Politecnico di Milano.
