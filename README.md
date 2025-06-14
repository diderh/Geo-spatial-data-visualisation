# Project Overview
This R script analyzes the spatial variability of nitrate and phosphate concentrations in Lower Saxony, Germany, focusing on the influence of land cover and cattle density. It integrates and processes geospatial data (catchments, nutrient concentrations, land cover, cattle density) to explore environmental patterns using advanced mapping, spatial joins, and statistical modeling.
---

# Features
  - Loads and processes multiple geospatial data formats (GPKG, RDS, GeoTIFF).
  - Filters and prepares data for Lower Saxony.
  - Aggregates nutrient concentrations and cattle density by catchment and land cover types.
  - Generates interactive and static maps using tmap and mapview.
  - Performs spatial joins, faceted visualization, and trend surface analysis.
  - Conducts linear regression to model relationships between cattle density, land cover, and nutrient concentrations.
  - Outputs various summary tables and publication-ready maps.

# Getting Started
  - Install required R packages: sf, dplyr, ggplot2, terra, tmap, leaflet, and others listed at the top of the script.
  - Ensure all data files (e.g., catchments_germany.gpkg, waterbase_prepared.rds, cattle_data.tif, landcover files) are available in the working directory.
  - Source the script in R or RStudio.

# Workflow Steps
1. **Load Libraries and Data:** Import spatial, statistical, and visualization libraries; load catchment, nutrient, cattle, and landcover data files.
2. **Preprocessing:** Filter data for Lower Saxony; mask and crop raster data; transform coordinate systems for alignment.
3. **Spatial Analysis:**
   - Aggregate nitrate and phosphate data by catchment.
   - Calculate mean cattle density per catchment.
   - Join nutrient data with catchment and land cover information.
   - Extract dominant land cover types for each catchment.
4. **Mapping:**
   - Create base and thematic maps showing spatial patterns of cattle density, nitrate and phosphate concentrations, and land cover types.
   - Use faceting to compare patterns across categories.
   - Generate Voronoi diagrams and trend surface maps for additional spatial context.
5. **Statistical Modeling:**
   - Perform linear regressions to analyze the effect of cattle density and land cover on nutrient concentrations.
   - Visualize relationships using scatterplots and regression diagnostics.

# Outputs
  - PNG maps showing:
  - Cattle density per catchment.
  - Spatial variability of nitrate and phosphate concentrations.
  - Nutrient levels explained by land cover and basin types.
  - Land cover distribution in Lower Saxony.
  - Summary tables of aggregated nutrient and land cover data.
  - Regression result summaries and predicted nutrient concentration plots.
  - CSV/DF outputs for further analysis (not explicitly saved in script but generated as data frames).
  - Diagnostic plots for regression models.

# Conclusion
The script provides a comprehensive workflow for assessing how land cover and cattle density influence water quality in Lower Saxony. By integrating spatial data, statistical modeling, and advanced visualization, it supports environmental assessment and management. The outputs facilitate understanding spatial patterns and drivers of nutrient pollution, aiding in targeted interventions.


![gis](https://github.com/user-attachments/assets/df85f04f-baab-414f-a430-a0db3ab1a8cb)





