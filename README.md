eyetrackingR
=====================================

## Eye-tracking Data: Cleaning, Analysis, & Visualization
#### Version: 0.0.0.9000


- Jacob Dink (jacobwdink@gmail.com)
- Brock Ferguson (brock.ferguson@gmail.com)


This package is designed to make dealing with eye-tracking data easier. It addresses tasks along the pipeline from raw data to analysis and visualization. It offers several popular types of analyses, including linear and growth curve time analyses, onset-contingent reaction time analyses, and bootstrapping cluster analyses, as well as novel non-parametric approaches to time-series data.

## Installation

Make sure you have the `devtools` package installed:

```
install.packages("devtools")
```

Then to install and load eyetrackingR, simply run:

```
devtools::install_github("jwdink/eyetrackingR")
library("eyetrackingR")
```

If you'd like the tutorials available for offline use (accessible with `browseVignettes("eyetrackingR")`), instead run:

```
devtools::install_github(build_vignettes = TRUE)
```

These vignettes will also be available on the project website, coming soon.

---

Copyright (c) 2015, Jacob Dink and Brock Ferguson

Released under the MIT License (see LICENSE for details)
