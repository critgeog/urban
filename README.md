# urban and suburban boundaries

This project establishes sub/urban boundaries. These boundaries offer one approach to visualize and analyze sub/urbanization in US metropolitan areas from 1940 to 2010. We use the Hammer Method to backcast estimates of housing units at the block group (or census tract) during each decade. The Hammer method uses historic county-level Census data (nhgis.org), and current county-level and block group (or census tracts) ACS data to estimate the number of housing units. After generating the estimate for each sub-geography in each time period, we normalize the data by dividing the number of units by the sub-geography area (square miles). The output, then, is the number of housing units per square mile within a given block group.

## Getting started
Aside from the area computation of the sub-geographies (completed in ArcGIS), all work is completed in RStudio. Most necessary data is available in the [data](data) folder. All scripts are located in the [scripts](scripts) folder.

Scott - The 1990 and 2000 county-level data is located in our shared dropbox. It is too large to be loaded to GitHub.

## Files - pertinent information

[UShammerMethodBG](scripts/UShammerMethodBG.R) is a complete script. It includes step-by-step calculations of the number of housing units per square mile in each block group, nationwide. 

This script reads in all data necessary to calculate adjusted housing units using the Hammer Method. All data is located in the 'data' folder. This script reads in 'Year Structure Built' block group data from the 2011-15 ACS block group data; a BG .txt file where block group areas were calculated in ArcGIS after reprojecting NHGIS data; and county-level historical counts of housing units (1950 and 1960 are from mhauer; all other decades were downloaded from NHGIS). 

Similarly, [UShammerMethodCT](scripts/UShammerCT.R) is modeled after this file, yields the number of housing units per square mile at the census tract level, nationwide. The steps are the same, but the results are aggregated to a different sub-geography. In this script, I do not read in each separate county-level historical counts of housing units, I just read in the file generated in [UShammerMethodBG](scripts/UShammerMethodBG.R). This single, complete historic housing unit file is located [here](csv/h_units_national.csv)
