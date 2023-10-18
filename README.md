# Population health implications of exposure to pervasive military aircraft noise pollution

This repository contains the reproducible methods and workflow routines used in the research article, "Population health implications of exposure to pervasive military aircraft noise pollution".

> DISCLAIMER: The code in this repository was developed for the purposes of this specific study. However, it can be used as a reference and freely redesigned for other purposes.

## Data availability

Refer to the Data Availability Statement in the paper to find links to requisite data, including:

-   Navy data from the NAVFAC database, exported from Larson Davis binary in xlsx format in the same directories as the original binary files.
-   NPS data from the National Park Service Night Skies and Sounds Division, in txt format.
-   JGL data from JGL Acoustics, Inc. in csv format.

Software and package dependencies are listed in `methods.Rmd`.

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
