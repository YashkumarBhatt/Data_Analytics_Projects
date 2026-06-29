# Data Analytics Projects

A collection of business intelligence and data analytics projects across 
HR, sales, environmental, and urban mobility domains — built using 
Power BI, Tableau, and R.

Each project folder contains the dashboard screenshot, dataset, and a 
project-specific README with context, metrics, and findings.

---

## Projects

### 📊 [HR Analytics Dashboard](./HR-Analytics-PowerBI)
Power BI dashboard tracking employee attrition across departments, salary 
bands, age groups, and job roles. Identifies that low-salary employees 
(≤5K) and the 26–35 age group drive the highest attrition.

`Power BI` `HR Analytics` `Attrition`

---

### 💰 [Annual Sales Analytics Dashboard](./Annual-Sales-KPIs-PowerBI)
Quarterly Power BI dashboard tracking retail sales performance — profit 
trends, category breakdown, and unit pricing across 10 product lines.

`Power BI` `Sales Analytics` `KPI Tracking`

---

### 👥 [Attrition Analysis](./Attrition-Analysis-Tableau)
Tableau dashboard visualizing workforce attrition patterns across gender, 
department, tenure, and income. Overall attrition rate: 16.12%.

`Tableau` `HR Analytics` · [Live Dashboard →](https://public.tableau.com/views/AttritionAnalysis_17553374431500/AttritionAnalysisDashboard)

---

### 🌍 [Global CO2 Emissions](./Global-CO2-Emissions-Tableau)
Tableau choropleth dashboard tracking CO2 emissions per capita and by 
region from 1960–2011. East Asia & Pacific surpasses all regions by 2011.

`Tableau` `Environmental Analytics` · [Live Dashboard →](https://public.tableau.com/views/DashboardsStarterTemplate_17326933787950/Dashboard1)

---

### 🚲 [Urban Mobility Trends — Cyclistic Bike-Share](./Urban-Mobility-Trends-R)
R-based analysis of 12 months of Chicago bike-share data (1.5GB+), 
auto-ingested from AWS S3. Compares casual vs. member rider behavior 
across volume, duration, and seasonality.

`R` `ggplot2` `EDA` `AWS S3`

---

## Stack

`Power BI` · `Tableau` · `R` · `ggplot2` · `tidyverse`

---

## Structure

Each project folder contains:

project-name/

├── Dashboard_Screenshot.png   # Visual output

├── Data.csv / Data.xlsx       # Dataset (where applicable)

└── README.md                  # Metrics, findings, and links
