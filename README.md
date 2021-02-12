---
editor_options: 
  markdown: 
    wrap: 80
---

# DWR-DET: Division of Water Rights Demand Exploration Tool

> Explore California's Surface Water Demands

## Descriptions of Key Files

-   `app.R`: The Shiny application.

-   `01-prep-wr-info.R`: Retrieves and preprocesses water right information
    publicly available from the [California Water Rights
    LIST](https://data.ca.gov/dataset/water-rights) available on the California
    Open Data Portal, and geospatial information. retrieves and preprocess point
    of diversion (POD) geospatial information publicly available from [eWRIMS
    GIS - Data
    Download](https://waterrightsmaps.waterboards.ca.gov/viewer/Resources/Images/eWRIMS/download.htm).

-   `02-prep-reported-diversions.R`: Retrieves and preprocesses reported
    diversions from the EWRIMS database to display reported diversion demand
    scenarios. Currently can only work for DWR staff who have access to the
    WRUDS Data Download link. currently these are the only demand datasets in
    tool.

-   `03-prep-historical-supplies.R`: Loads and preprocesses monthly historical
    supply data at gage stations in several HUC-8 Watersheds. dataset maintained
    and curated by DWR staff?

## Motivation

[under development]
