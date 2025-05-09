SPDX-License-Identifier: GPL-2.0-or-later

# Flexible HCPC & PCA Biplot Analysis

This Shiny app performs HCPC (hierarchical clustering on principal components) and PCA biplot analysis on uploaded Excel data.

## ⚙️ Prerequisites
- R ≥ 4.0  [![R ≥ 4.0](https://img.shields.io/badge/R-%3E%3D%204.0-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)  
- Packages:
  - `shiny` (GPL-3)
  - `shinythemes` (GPL-3)  
  - `ggrepel` (GPL-3) 
  - `FactoMineR` (GPL-2 | GPL-3)  
  - `factoextra` (GPL-2)
  - `ggbiplot` (GPL-2)
  - `readxl` (MIT) 
  - `dplyr` (MIT)  
  - `tibble` (MIT)  
  - `ggplot2` (MIT)
  - `viridis` (MIT) 

## 🚀 Usage
1. **Clone the repo**  
   ```bash
   git clone https://github.com/<you>/flexible-hcpc-pca-app.git
   cd flexible-hcpc-pca-app

2. **Start the app in R**
    ```bash
    shiny::runApp("app_1.2.R")

3. **Prepare your input Excel file**
Your spreadsheet must have three distinct sections, in this order:

| Section             | Description                                                                               |
| ------------------- | ----------------------------------------------------------------------------------------- |
| **Row labels**      | First column: unique IDs (e.g. gene or sample names)                                      |
| **Row annotations** | One or more columns immediately after: categorical factors (e.g. “genotype”, “condition”) |
| **Numeric data**    | All remaining columns: measurement values; headers are variable names (e.g. metabolites)  |
