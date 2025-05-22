# Big Data Analysis

## Overview

This project focuses on analyzing brand awareness and customer perception using survey data. Utilizing R, it encompasses data cleaning, transformation, and statistical analysis to derive actionable insights into brand equity and Net Promoter Score (NPS).

## Project Structure

### Data Files

- `main_data.csv`: Primary dataset containing survey responses.
- `brand_equity.csv`, `awareness_analysis.csv`, `nps_analysis.csv`: Processed datasets for specific analyses.
- `Regional.Structure.csv`, `RegionC.csv`: Regional segmentation data.
- `Kwestionariusz.Grupa.4.docx`: Original survey questionnaire in Polish.

### R Scripts

- `clean_transform.R`: Data cleaning and preprocessing.
- `My_Functions.R`: Custom utility functions.
- `brand_awareness.R`: Analysis of brand awareness.
- `brand_euity.R`: Evaluation of brand equity metrics.
- `nps.R`: Calculation and interpretation of Net Promoter Score.
- `bootstrap.R`: Statistical inference using bootstrap methods.
- `Packages.R`: Required R packages for the project.

### Project Files

- `big_data_analysis.Rproj`: RStudio project file.
- `renv/`, `renv.lock`: Environment files for dependency management.

## Setup Instructions

1. **Clone the repository**:

   ```bash
   git clone https://github.com/paulakora/big_data_analysis.git
   ```

2. **Open the project in RStudio**:
   Open the file `big_data_analysis.Rproj`.

3. **Install dependencies**:
   Run the following in R to install the required packages:

   ```r
   source("Packages.R")
   ```

4. **Restore the environment (optional but recommended)**:
   If using `renv`, restore the environment with:

   ```r
   renv::restore()
   ```

## Analysis Workflow

1. **Data Cleaning**:  
   Run `clean_transform.R` to clean and format raw survey data.

2. **Utility Functions**:  
   Source `My_Functions.R` to access helper functions.

3. **Brand Awareness**:  
   Run `brand_awareness.R` to analyze brand recognition and recall.

4. **Brand Equity**:  
   Use `brand_euity.R` to evaluate brand strength components.

5. **Net Promoter Score (NPS)**:  
   Execute `nps.R` to calculate and interpret NPS values.

6. **Bootstrap Analysis**:  
   Use `bootstrap.R` for confidence intervals and hypothesis testing via bootstrapping.

## Data Sources

- Survey responses collected with the form in `Kwestionariusz.Grupa.4.docx`.
- Regional data in `Regional.Structure.csv` and `RegionC.csv`.
