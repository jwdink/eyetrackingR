# eyetrackingR 

Changes in 0.1.1.9000:

* Added (experimental) support for `glm` and `glmer` in `analyze_time_bins` and therefore in cluster-analyses. Email any bug-reports to jacobwdink@gmail.com
* Added `simulate_eyetrackingr_data` function to generate fake data for simulations.
* Added *very* experimental support for `boot-splines` method within cluster analysis (investigating hybrid method, not meant for serious analyses yet).

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