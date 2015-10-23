eyetrackingR
=====================================

## Eye-tracking Data: Cleaning, Analysis, & Visualization
#### Version: 0.0.1.0000


- Jacob Dink (jacobwdink@gmail.com)
- Brock Ferguson (brock.ferguson@gmail.com)


This package is designed to make dealing with eye-tracking data easier. It addresses tasks along the pipeline from raw data to analysis and visualization. It offers several popular types of analyses, including growth-curve analysis, onset-contingent reaction time analyses, as well as several non-parametric bootstrapping approaches.

www.eyetracking-r.com

## Installation

We are in the process of preparing eyetrackingR for CRAN. Until then:

Make sure you have the `devtools` package installed:

```
install.packages("devtools")
```

Then to install and load eyetrackingR, simply run:

```
devtools::install_github("jwdink/eyetrackingR")
library("eyetrackingR")
```

---

## Usage

EyetrackingR only requires that your data is in an R dataframe and has a few necessary columns. For that reason, **eyetrackingR is compatible with any eyetracker,** so long as you can export your data to a table and import it into R. See the [preparing your data](http://www.eyetracking-r.com/vignettes/preparing_your_data) vignette.

Once your data is in R, you  can prepare it for eyetrackingR by running the `make_eyetrackingr_data` function, e.g.:

```
data <- make_eyetrackingr_data(your_original_data, 
                       participant_column = "ParticipantName",
                       trial_column = "Trial",
                       time_column = "Timestamp",
                       trackloss_column = "TrackLoss",
                       treat_non_aoi_looks_as_missing = TRUE
)
```

From here, all of eyetrackingR's functionality becomes available for this data. Check out [the eyetrackingR workflow](http://www.eyetracking-r.com/workflow) to get an accesible overview of this functionality, or check out [the vignettes](http://www.eyetracking-r.com/vignettes) for guides on how to clean your data, visualize it, and perform analyses. 

***

Copyright (c) 2015, Jacob Dink and Brock Ferguson

Released under the MIT License (see LICENSE for details)
