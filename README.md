# COSEWIC-prioritize

Work in progress repository developed for the marine fishes subcommittee of the Committee on the Status of Endangered Wildlife in Canada ([COSEWIC](https://www.cosewic.ca/index.php/en-ca/)) to support prioritization of Pacific salmon Designatable Units (DUs) for status assessments. 

Repository sources and wrangles data on Pacific salmon spawner abundance (mature individuals) over time from various sources (see [Data sources](https://github.com/Pacific-salmon-assess/COSEWIC-prioritize/blob/main/README.md#data-sources)), calculate rates of change in individuals over time at the DU scale, estimates percent change over the most recent three generations, and summarizes this information in an html summary document. 

## Repository strucutre
- `probable-status.Rmd`: Sources data, wrangles it, calculates rates of change by DU, summarizes probabale status and associated DU level metadata. Renders html document.
- `data`: DU metadata and associated timeseries of mature individuals. Sourced from [here](https://github.com/hertzPSF/COSEWIC-compilation), origional data sources detailed in [data sources](https://github.com/Pacific-salmon-assess/COSEWIC-prioritize/blob/main/README.md#data-sources).
- `output`: All outputs including master .csv of percent change in mature individuals and probable DU status designations, stand alone plots of mature individuals over time by species and region.

If you just want to explore a summary of probable designations based on currently available data then download the `probable-status.html` file. The master table of probable (or COSEWIC assessed) status, rates of change, etc., all by DU can be found here: `./output/master-prob-status/master-status.csv`.

To recreate the summary document (e.g., with updated data) open the `probable-status.Rmd` in Rstudio and then click the **knit** button.

## Data sources
Data on mature individuals, DU-CU mapping, and associated metadata are sourced from a [repository](https://github.com/hertzPSF/COSEWIC-compilation) with the data in the [Pacific Salmon Explorer](https://www.salmonexplorer.ca/) and other published sources (e.g., Yukon Chinook). Origional sources of data are detailed below. 
| **Region (species)** | **Source** |
| --- | --- |
| Yukon (Chinook) | Connors BM, Siegle MR, Harding J, Rossi S, Staton B, Jones M, Bradford M, Browne R, Bechtol B, Doherty B and S Cox. Chinook salmon diversity contributes to fishery stability and trade‐offs with mixed‐stock harvest. In press. Ecological Applications. [[link](https://github.com/brendanmichaelconnors/yukon-chinook-diversity)] |
| Nass (pink, chum, coho, Chinook) | Connors K, Hertz E, Jones E, Honka L, Kellock K, and R Alexander. 2019. The Nass Region: Snapshots of salmon population status. The Pacific Salmon Foundation, Vancouver, BC, Canada.[[link](https://salmonwatersheds.ca/libraryfiles/lib_453.pdf)]|
| Skeena/Nass (sockeye) | Pestal GP, Carr-Harris C, Cox-Rogers S, English K, Alexander R and the Skeena Nass Sockeye Technical Working Group. 2022. 2021 Review of Spawner and Recruit Data for Sockeye Salmon (*Oncorhynchus nerka*) from the Skeena and Nass Basins, British Columbia. Can. 20 Tech. Rep. Fish. Aquat. Sci. [[link]()] |
|Skeena (pink, chum, coho, Chinook) | English K, Peacock D, Challenger W, Noble C, Beveridge I, Robichaud D, Beach K, Hertz E and K Connors. 2018. North and Central Coast Salmon Escapement, Catch, Run Size and Exploitation Rate Estimates for each Salmon Conservation Unit for 1954-2017. [[link](https://salmonwatersheds.ca/libraryfiles/lib_451.pdf)]|
|Haida Gwaii (all species)      Central Coast (all species)| English K, Peacock D, Challenger W, Noble C, Beveridge I, Robichaud D, Beach K, Hertz E and K Connors. 2018. North and Central Coast Salmon Escapement, Catch, Run Size and Exploitation Rate Estimates for each Salmon Conservation Unit for 1954-2017. [[link](https://salmonwatersheds.ca/libraryfiles/lib_451.pdf)]|
|Central Coast (all species) | English K, Peacock D, Challenger W, Noble C, Beveridge I, Robichaud D, Beach K, Hertz E and K Connors. 2018. North and Central Coast Salmon Escapement, Catch, Run Size and Exploitation Rate Estimates for each Salmon Conservation Unit for 1954-2017. [[link](https://salmonwatersheds.ca/libraryfiles/lib_451.pdf)]|

## Prioritization decison nodes
Initial thoughts on key decision nodes in prioritization process. 
- **Have DUs been formally defined by COSEWIC?** Chinook, sockeye, and coho salmon DUs have been defined (reports available [here](https://www.cosewic.ca/index.php/en-ca/reports/special-reports.html)), formal pink and chum DU identification is in progress. If **yes** then proceed.   
- **Is information on mature individuals over time available at the DU scale?** If **yes** then proceed.
- **Has DU been previously assessed by COSEWIC?** If **yes** then consider a re-assessment, otherwise proceed.
- **What is the DUs probable designation based on percentage change in mature individuals over past three generations?** If **threatened** or **endangered** then proceed.
- **Are there spatial considerations?** Are there commonalities in threats, designations, and/or data landscape at a regional scale that suggest a group of DUs should be bundled together?  
