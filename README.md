## urban and suburban classification


###### [UShammerMethodBG](scripts/UShammerMethodBG.R) is a complete script. It includes step-by-step calculations of the number of housing units per square mile in each block group, nationwide. 

This script reads in all data necessary to calculate adjusted housing units using the Hammer Method. All data is located in the 'data' folder. This script reads in 'Year Structure Built' block group data from the 2011-15 ACS block group data; a BG .txt file where block group areas were calculated in ArcGIS after reprojecting NHGIS data; and county-level historical counts of housing units (1950 and 1960 are from mhauer; all other decades were downloaded from NHGIS). 

Similarly, [UShammerMethodCT](scripts/UShammerCT.R) is modeled after this file, yields the number of housing units per square mile at the census tract level, nationwide. The steps are the same, but the results are aggregated to a different sub-geography. In this script, I do not read in each separate county-level historical counts of housing units, I just read in the file generated in [UShammerMethodBG](scripts/UShammerMethodBG.R). This single, complete historic housing unit file is located [here](csv/h_units_national.csv)

##### does this work now?