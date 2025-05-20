# S&P 500 Returns

## Overview
Interactive Shiny app analyzing S&P 500 historical data (1928-2023) with:
- Adjustable time period visualization
- Multi-year return calculations
- Interactive statistical summaries

## Features
**Plot 1**: Daily Closing Values  
- Line graph of S&P 500 closing prices
- Adjustable date range (1928-2023)
- Linear/logarithmic scale toggle

**Plot 2**: N-Year Returns  
- Bar chart of percentage returns
- Configurable year window (1-95 years)
- Optional statistical overlays:
  - Mean (red)
  - Median (blue) 
  - Standard deviation (orange)

**Summary Table**:  
- Key statistics for selected period:
  - Percentiles (10th, 25th, 75th, 90th)
  - Mean/Median returns
  - Standard deviation

## How to Run
1. Ensure required packages are installed:
```R
install.packages(c("shiny", "tidyverse", "lubridate", "tidyquant", "plotly"))
```
  - Place all files in the same directory
  - Run the app
```R
shiny::runApp("sp500returns.R")
```
