# COSEWIC-prioritize

Code to:
- source and wrangle data on Pacific salmon spawner abundance (mature individuals) over time from various sources (add table)
- calculate rates of change in mature individuals over time and and estimate % change over specified time period to support prioritization of Designatable Units for status assessments by COSEWIC

## Repository strucutre
- `probable-status.Rmd`: Sources data, wrangles it, calculates rates of change by DU, summarizes probabale status and associated DU level metadata. Renders html document.
- `data`: DU-CU metadata and associated timeseries of mature individuals. Sourced from [ere](https://github.com/hertzPSF/COSEWIC-compilation), origional data sources detailed in table below.
- `outputs`: All outputs including master .csv of percent change in mature individuals and probable DU status designations, stand alone plots of mature individuals over time by species and region.
