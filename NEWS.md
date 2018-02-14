# eyetrackingR 

Changes in 0.1.7:
* Compatible with dplyr > 0.5.0.  
* Fixes issue described in https://github.com/jwdink/eyetrackingR/issues/57 
* Fixes bug in add_aoi when only one AOI is added.  

Changes in 0.1.6:
* Allows for treatment-coded variables in `lm` or `lmer` time-bin or cluster analysis, via the "treatment_level" argument.

Changes in 0.1.5:
* Fixes compatibility issue with latest version of `lme4` package.

Changes in 0.1.4:
* A variety of important bug-fixes for onset-contingent analysis. The rest of the package is unchanged.

Changes in 0.1.3:

* The `analyze_time_bins` and therefore cluster-analyses have been re-written internally. Full support for (g)lm, (g)lmer, wilcox. Support for interaction terms/predictors. Experimental support for using boot-splines within cluster analysis.
* P-value adjustment for multiple comparisons is now supported in `analyze_time_bins`
* Easier to use AOI as a predictor/covariate in `analyze_time_bins` and cluster analyses
* The functions `make_boot_splines_data` and `analyze_boot_splines` are now deprecated. To perform this type of analysis, use `test="boot_splines"` in `analyze_time_bins`. 
* Warnings and errors are now given in the returned dataframe for `analyze_time_bins`.
* Fixed plotting methods for time-cluster data
* The `analyze_time_clusters` function now checks that the extra arguments passed to it are the same as the arguments passed
* Fixed small bug in make_onset_data
* Added `simulate_eyetrackingr_data` function to generate fake data for simulations.

Changes in 0.1.1:

* **Important** bug-fix in `clean_by_trackloss`. Previously did not work for certain column names.
* **Important** bug-fix in `make_eyetrackingr_data`. Previously did not work correctly with `treat_non_aoi_as_missing = TRUE`.
* **Important** bug-fix in `analyze_time_clusters`: previously did not compute permutation-distribution correctly.
* Can specify any arbitrary dependent-variable for `make_time_window_data` or `make_time_sequence_data` to summarize. This DV can then be plotted and used in downstream functions (like `analyze_time_bins` or `make_time_cluster_data`)
* Bug-fix in error/warning reporting in `analyze_time_bins` and functions that call this (e.g `make_time_cluster_data`).
* Compatible with ggplot2 2.0
* Small bug fix in cluster analyses functions related to the dots (...) arguments
* Added support for parallelization in `analyze_time_clusters`, allowing the user to take advantage of multiple cores to speed up this relatively slow function.
* Added `get_time_clusters` for getting information about clusters in a data.frame (rather than a printed summary-- better for programming).
* Small bug-fixes in make-boot-splines.
* Changed how cluster-summaries are displayed