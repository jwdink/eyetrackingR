# eyetrackingR 0.1.0.9000

Current development version changes (since 0.1.0 release):

* Can specify any arbitrary dependent-variable for `make_time_window_data` or `make_time_sequence_data` to summarize. This DV can then be plotted and used in downstream functions (like `analyze_time_bins` or `make_time_cluster_data`)
* Bug-fix in error/warning reporting in `analyze_time_bins` and functions that call this (e.g `make_time_cluster_data`).