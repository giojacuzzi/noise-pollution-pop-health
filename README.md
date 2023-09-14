# PHI-Noise-Pollution

Data analysis tools for UW Population Health Initiative - "Human health and well-being implications of pervasive Navy aircraft noise pollution"

## Contents

See sections below corresponding to repository directories. `global.R` contains global variables, functions, and settings used throughout the repository.

### /analysis

Analyses of acoustic index metrics and spatiotemporal distribution of noise.

#### Quantitative characterization of noise

-   Individual noise event data for all SPL sites - `noise_characterization.R`
    -   Level magnitude (LAmax, LCpeak, SEL)
    -   Spectrum (for Navy and some JGL events only)
        -   Average 1/3 octave band spectrum for FCLP events for sites near Coupeville for an example session
    -   Timing (duration, onset rate, timestamp)
-   Acoustic index metrics for Navy, and NPS sites (not including Lopez and PT) - `level_index_averages.R`
    -   Ldn, Lden, Leq, etc.
    -   Comparison between measured Ldn, Navy-reported DNL, and modeled DNL

#### Temporal distribution of noise

-   Acoustic index metrics for Navy monitoring periods per...
    -   Day of week, hour of day - `temporal_distribution.R`
    -   Days/nights of activity vs inactivity (weekdays vs weekends) - `level_index_averages.R`
    -   Season
-   Number of flight operations for Navy monitoring periods per...
    -   Day of week, hour of day - `temporal_distribution.R`
-   Total FCLP operations per month of 2021-2022, grouped by time of day

#### Spatial distribution of noise

-   Modeled average noise exposure maps of 4 Navy monitoring periods of 2020 (via DoD BaseOps) `map.R`
    -   DNL (equivalent to Ldn, and a conservative approximation of Lden)
    -   Lday, Lnight, Leq24

#### Health impacts

-   Spatial population impacts - `map.R`
    -   Annoyance (also `annoyance.R`)
    -   Sleep disturbance (also `sleep_disturbance.R`)
    -   Hearing impairment TODO
    -   Noise guidelines, ordinances, land-use compatibility TODO
    -   Cognitive development and childhood learning (TODO, also `cognitive_development_and_speech.R`)
        -   Per-school exposure levels
    -   Cardiovascular impacts TODO
        -   Relative risk (we need to know the baseline cardiovascular risk to compute this, see Babish)
    -   Noise complaints
-   Site-specific impacts TODO
    -   OSHA/NIOSH violations TODO
-   TODO: "turning the knob", simulate reducing flight activity by X% to achieve Y reduction in population impacts

### /data

All data processing routines. Includes some raw metadata, but is dependent on the PHI database being mounted at the path defined in `global.R`.

`pipeline.R` contains the entire pipeline that processes raw data from database files into acoustic index metrics and events. Subdirectories are:

-   `load` - load raw audio data from a database file corresponding to a given site-date
-   `events` - identify individual noise events
-   `metrics` - definition and calculation of acoustic index metrics
-   `flight_ops` - load flight operations and BaseOps modeling data
