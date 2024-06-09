# GW-obs-point-elev-QC

This code is a part of the quality control work of physical structure elevation values in the hydrological groundwater monitoring network of [the Finnish Environment Institute](https://www.syke.fi/en-US).
Thoroughly verifying and updating the existing elevation values is required to set a reliable standard to the data produced by the network. 
The code utilises precise field measurements of the structures, as well as national elevation datasets to verify the current coordinate and elevation values of the structures in the database.
The national elevation datasets are [KM2](https://www.maanmittauslaitos.fi/en/maps-and-spatial-data/datasets-and-interfaces/product-descriptions/elevation-model-2-m) and [lidar 5p/m^2^](https://www.maanmittauslaitos.fi/en/maps-and-spatial-data/datasets-and-interfaces/product-descriptions/laser-scanning-data-5-p), both provided by [the National Land Survey of Finland](https://www.maanmittauslaitos.fi/en).
Confidence intervals of the elevation sources are also evaluated based on the precise field measurements.
New elevation values are proposed to replace database values if multiple sources suggest a new value, with the database value outside of the confidence intervals of the other sources.

The funding for this project has come from [Maa- ja vesitekniikan tuki ry](https://mvtt.fi/), through the POVEMU project of the Finnish Environment Institute.

---
## Running the script
The core of the script is in `00-base.Rmd`, which can be run using [RStudio](https://posit.co/download/rstudio-desktop/). Supporting functions and sub-tasks of the script are located in the folder `/R/`.
For optimal functionality, the code requires internet access, access to the groundwater database of the Finnish Environment Institute, as well as access to the full lidar dataset.
Most of the source data used for this project cannot be provided due to restricted access, but some of it is openly available from the respective sources.
The dependencies of the project are listed at the start of the code, as well as in DESCRIPTION. The required missing CRAN packages should get installed automatically if the script is run.

---
## Notes
The code has been created for data-analysis, where the results will be primarily utilised once, by uploading the proposed new elevations to the database.
Thus, streamlining and optimising the code has not been the priority, although some performance re-writes were required to speed up the process of creating DTMs from the lidar data.

The code has been developed by Pietari Pöykkö (ORCID: 0009-0000-1214-5336), while employed at the Finnish Environment Institute.
The Finnish Environment Institute holds the copyright to this project. See LICENSE for more.
