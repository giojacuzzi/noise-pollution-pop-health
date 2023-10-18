# Population health implications of exposure to pervasive military aircraft noise pollution

Data analysis tools for UW Population Health Initiative - "Human health and well-being implications of pervasive Navy aircraft noise pollution".

> Disclaimer: Note that the following code is developed for this specific study purpose, but is not generally plug and play. It can be repurposed for future studies, and should be used a reference.

## Data availability

TODO: reference Data Availability Statement in manuscript.

Raw data should be 1-sec with X metrics.

-   Navy data from ... exported from Larson Davis binary in xlsx format in the same directories as the original binary files.
-   NPS data from ... in txt format with ...
-   JGL data from ... in csv format with file name convention `ID_YYYY-MM-DD`, where ID is the site ID corresponding to a site listed in `data/load/sites/sites.csv`.

<!-- -->

-   `NAVY` - The NAVFAC database, with acoustic data files converted from Larson Davis binary format .LD0 to .xlsx, including operational data and [Noisemap files](https://www.navfac.navy.mil/Portals/68/Documents/Business-Lines/Asset-Management/Sound/Remaining-Adds/PUBLIC_NOISEMAP.zip?ver=KEbUPIKWwvnjZl0H4vUg9g%3d%3d).
-   `NPS` - Acoustic data from the National Park Service (.txt).
-   `JGL` - Acoustic data from JGL Acoustics, Inc (.csv).
-   `GIS` - GIS data, including subdirectories:
    -   `NCLD` - [US National Land Cover Database](https://www.mrlc.gov/data/nlcd-2019-land-cover-conus)
    -   `NCES` - [NCES school location shapefiles](https://nces.ed.gov/programs/edge/geographic/schoollocations)

In addition to the raw data listed above, software and package dependencies are listed in `methods.Rmd`.

## Contents

The full data processing and analysis pipeline described in the "Materials and Methods" section of the paper can be found at `methods.Rmd`. `global.R` contains global variables, functions, and settings used throughout the repository. See sections below corresponding to repository directories.

### analysis

Noise regime

-   Single event acoustic metrics
-   Cumulative acoustic metrics
-   Temporal distribution of noise

Population noise exposure

-   Dasymetric population density estimation
-   Health risks associated with published thresholds

Population health impacts

-   Exposure-response relationship estimates
-   Childhood learning estimates
-   Hearing impairment doses

### data

All data and metadata loading, manipulation, and processing scripts. Dependent on the database being mounted at the path defined in `global.R` and containing the prerequisites listed in the "Data availability" section above.

### figures

Scripts for the generation of figures presented in the paper and supplementary materials.

### metrics

Helper functions and constants for the calculation of acoustic metrics and health impacts, including exposure-response functions and thresholds.

### simulation

BaseOps cases and associated scripts for Noisemap simulations.
