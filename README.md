# urban and suburban boundaries

This project establishes sub/urban boundaries. These boundaries offer one approach to visualize and analyze sub/urbanization in US metropolitan areas over time, from 1940 to 2010. We use the Hammer Method to backcast estimates of housing units at the block group (or census tract) during each decade. 

The Hammer method uses historic county-level Census data (Minnesota Pop Center, 2011), in combination with current county-level and block group (or census tracts) ACS data to estimate the number of housing units. We use these three data components to generate an estimated number of housing units in each sub-geography for each time period. Then, we normalize the number across space by dividing the number of housing units in each sub-geography by the the sub-geography area (square miles). The output, then, is the number of housing units per square mile within a given block group.

## Getting started

### Files 
Aside from computing the sub-geographies in ArcGIS, all work contained in this project is completed in RStudio. Most necessary data is available in the [data](data) folder. All scripts are located in the [scripts](scripts) folder. Maps and a .gif of urbanization in Atlanta from 1940 to 2010 are available in the [maps](maps) folder

Scott - The 1990 and 2000 county-level data is located in our shared dropbox. It is too large to be loaded to GitHub.

#### Scripts
There are three primary files:
1. [h_units.R](scripts/h_units.R): This script reads in, cleans, and combines the separate county-level housing units from 1940-2010 into a single file [h_units_national.csv](data/h_units_national.csv)

2. [bg_hammer.R](scripts/bg_hammer.R) : This script reads in data and calculates the number of housing units per square mile by block group, nationwide, using the Hammer Method. Script reads in 'Year Structure Built' block group data from the 2011-15 ACS, block group area in square miles, and reads in h_units_national.csv to join. This script also produces the maps and .gif found in [maps](maps/) folder.

3. [ct_hammer.R](scripts/ct_hammer.R) : This script reads in data and calculates the number of housing units per square mile by census tract, nationwide, using the Hammer Method.  Script reads in 'Year Structure Built' census tract data from the 2011-15 ACS, census tract area in square miles, and reads in h_units_national.csv to join. I did not make visualizations based on the census tract estimates.


Finally, [UShammerMethodBG](scripts/UShammerMethodBG.R) is a complete script. Essentially, it is a combination of h_units.R and bg_hammer.R where I did my initial analysis. I separted it into two scripts to keep hopefully make things a bit easier to follow. It includes step-by-step calculations of the number of housing units per square mile in each block group, nationwide. You shouldn't need this, but if you encounter problems in scripts #1 or #2 listed above, this is a good place to look.

This script reads in all data necessary to use the Hammer Method to calculate adjusted housing units.Data is located in the 'data' folder. This script reads in 'Year Structure Built' block group data from the 2011-15 ACS block group data; a BG .txt file where block group areas were calculated in ArcGIS after reprojecting NHGIS data; and county-level historic housing units (1950 and 1960 are from matt hauer; all other decades were downloaded from NHGIS). 

Similarly, [UShammerMethodCT](scripts/UShammerCT.R) is modeled after its BG counterpart, instead yielding the number of housing units per square mile at the census tract level, nationwide. The steps are the same, but the results are aggregated to a different sub-geography. In this script, I do not read in each separate historic count of county-level housing units; I just read in the file generated in [h_units.R](scripts/h_units.R). 


### additional notes

At this point, the Hammer Method component of the project is complete. Estimated housing units over time, based on Hammer, are available over time and across space. I also create a dummy variable to signal if it is considered 'urban' for each time period. I classify each sub-geography as 'urban' if it meets a threshold of > 200 housing units per square mile, or if the previous decade was classified as urban. Thus, once a block group 'becomes urban,' it remains so moving forward.


Additional factors, such as airports, parks, satelite imagery, and additional  dimensions of 'urbanity' to include in this framework are up to you. I have a national file of airports that I can share, if you think that might be helpful.

