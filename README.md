# Population health implications of exposure to pervasive military aircraft noise pollution

This repository contains the reproducible methods and workflow routines used in the research article:

> Jacuzzi G, Kuehne LM, Harvey A, Hurley C, Wilbur R, Seto E, Olden JD. Population health implications of exposure to pervasive military aircraft noise pollution. *Journal of Exposure Science and Environmental Epidemiology*. In press, 2024.

While this code was developed for the objectives of this specific study, it can be freely repurposed for other noise monitoring and health assessment initiatives. Please cite the original publication in your references and direct any correspondance to gioj@uw.edu.

## Data availability

Refer to the Data Availability statement in the paper for links to requisite data, including:

-   Navy acoustic data from the NAVFAC database, exported from Larson Davis binary in xlsx format in the same directories as the original binary files.
-   NPS acoustic data from the National Park Service Night Skies and Sounds Division, in txt format.
-   JGL acoustic data from JGL Acoustics, Inc. in csv format.

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

Utility functions and constants for the calculation of acoustic metrics and health impacts, including exposure-response functions and thresholds.

### simulation

BaseOps cases and associated scripts for Noisemap simulations.
