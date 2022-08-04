# COSEWIC-prioritize

Work in progress repository developed for the marine fishes subcommittee of the Committee on the Status of Endangered Wildlife in Canada ([COSEWIC](https://www.cosewic.ca/index.php/en-ca/)) to support prioritization of Pacific salmon Designatable Units (DUs) for status assessments. This repository specifically sources and wrangles data on Pacific salmon spawner abundance (mature individuals) over time from various sources (see [Data sources](https://github.com/Pacific-salmon-assess/COSEWIC-prioritize/blob/main/README.md#data-sources)), calculate rates of change in individuals over time, estimates percent change over the most recent three generations, and summarizes this information in an html summary document. 

## Repository strucutre
- `probable-status.Rmd`: Sources data, wrangles it, calculates rates of change by DU, summarizes probabale status and associated DU level metadata. Renders html document.
- `data`: DU-CU metadata and associated timeseries of mature individuals. Sourced from [here](https://github.com/hertzPSF/COSEWIC-compilation), origional data sources detailed in [Data sources](https://github.com/Pacific-salmon-assess/COSEWIC-prioritize/blob/main/README.md#data-sources).
- `outputs`: All outputs including master .csv of percent change in mature individuals and probable DU status designations, stand alone plots of mature individuals over time by species and region.

## Data sources
| **Region (species)** | **Source** |
| --- | --- |
| Yukon (Chinook) | Connors BM, Siegle MR, Harding J, Rossi S, Staton B, Jones M, Bradford M, Browne R, Bechtol B, Doherty B and S Cox. Chinook salmon diversity contributes to fishery stability and trade‐offs with mixed‐stock harvest. In press. Ecological Applications. [link](https://github.com/brendanmichaelconnors/yukon-chinook-diversity) |
| Skeena/Nass (sockeye) | Pestal, GP, C Carr-Harris, S Cox-Rogers, K English, R Alexander and the Skeena Nass Sockeye Technical Working Group. 2022. 2021 Review of Spawner and Recruit Data for Sockeye Salmon (*Oncorhynchus nerka*) from the Skeena and Nass Basins, British Columbia. Can. 20 Tech. Rep. Fish. Aquat. Sci. [link]() |
