## Project Description
This project investigates regional socio-demographic and economic data from Romania using two advanced multivariate techniques: factor analysis and correspondence analysis. The goal is to identify latent dimensions that explain variability in regional indicators and to explore relationships between age group distributions and geographical regions.

## Objectives
Assess the suitability of the dataset for factor analysis using KMO and Bartlett’s tests
Determine the optimal number of factors via Scree Plot, Kaiser’s criterion, and cumulative variance
Compare two extraction methods: Principal Axis Factoring and Maximum Likelihood
Interpret factor loadings and name latent dimensions
Apply correspondence analysis to examine the association between age groups and counties
Analyze contribution, inertia, and representation quality of variables in reduced-dimensional space

## Methodology
Factor analysis was conducted on standardized socio-economic indicators (e.g., education, health, commerce, infrastructure). Results were interpreted through rotated loadings and factor score plots. Correspondence analysis was applied to a contingency table of population age groups by county, highlighting regional demographic patterns.

## Technologies & Libraries
R – statistical computing environment
psych – for factor extraction and diagnostic tests
FactoMineR, factoextra – for correspondence analysis and visualization
GPArotation, corrplot, ggplot2 – for rotations and plotting

## Data Overview
The dataset includes:
County-level indicators (e.g., education, trade, public administration, health, etc.)
Age distribution by county (2021 census)
Data was sourced from the Romanian National Institute of Statistics.

## Main Findings
A dominant socio-economic factor explains a significant portion of variance
Factor models highlight clear thematic structures (education, infrastructure, public services)
Correspondence analysis reveals associations between specific age groups and counties, showing distinctive demographic profiles
Over 90% of variance is captured by the first two dimensions in both analyses, ensuring robust interpretation

## Author
Amalia Lică


