# Birth Weight Analysis

## Project Overview
This project analyzes birth weight data using R, with a focus on the relationship between maternal smoking and birth weight. The analysis includes data preprocessing, hypothesis testing, and visualization to explore patterns and statistical significance.

## Features
- Data cleaning and preprocessing, including factor conversion
- Summary statistics for numerical variables
- Hypothesis testing:
  - T-Test and Wilcoxon Test for birth weight comparison
  - Chi-Square and Fisher’s Exact Test for categorical relationships
- Data visualization with boxplots, histograms, and QQ-Plots
- Statistical power simulation with 10,000 hypothesis tests

## Technologies Used
- R Programming
- Data analysis with tidyverse
- Statistical testing with gmodels
- Visualization with ggplot2, gridExtra, and patchwork

## Installation and Usage
1. Install required R packages if they are not already installed:
   ```r
   install.packages(c("tidyverse", "gmodels", "gridExtra", "patchwork", "knitr", "kableExtra"))
   ```
2. Load the dataset by running the script and selecting a CSV file when prompted.
3. Review summary statistics and hypothesis testing results.
4. Visualizations and statistical results will be displayed in the R console.

## Insights Gained
- Smoking mothers had lower average birth weights compared to non-smokers.
- Statistical power analysis confirmed the effectiveness of different tests.
- The Wilcoxon test provided alternative insights where normality assumptions did not hold.

## Future Improvements
- Expand the dataset to include additional maternal health factors
- Apply machine learning models for predictive analysis
- Automate data preprocessing for larger datasets

## Contact
For any questions or discussions, feel free to reach out via GitHub or LinkedIn.

