#' Prevent accidental plotting of data.frames whose class has been removed.
#'
#' Eyetracking R adds a plot method to most dataframes you can create with it.
#' Sometimes, the class of the dataframe can be removed, so that plot will
#' not have the expected result. This ensures a warning is issued to the user.
#' 
#' @param data
#' @export
#' @return NULL
plot.data.frame <- function(data) {
  stop("Cannot plot this data. Either no plotting method exists for this data, or the class of this data, which specifies ",
       "what type of data it is, has been removed. This can happen by using functions that transform the data significantly, ",
       "such as dplyr's 'summarize' and 'select'.")
}

#' .make_proportion_looking_summary()
#'
#' A helper function for make_time_window_data and make_time_sequence_data. Takes a dataframe, groups it, and
#' returns proportion looking and relevant transformations
#'
#' @param data
#' @param groups
#' @param aoi_col
#' @return dataframe
.make_proportion_looking_summary <- function(data, groups, aoi_col) {
  # Group, summarize Samples
  df_grouped <- group_by_(data, .dots = groups)
  df_summarized <- summarize_(df_grouped,
                             .dots = list(SamplesInAOI = interp(~sum(AOI_COL, na.rm=TRUE), AOI_COL = aoi_col),
                                          SamplesTotal = interp(~sum(!is.na(AOI_COL)), AOI_COL = aoi_col) # ignore all NAs
                             ))

  # Calculate Proportion, Elog, etc.
  aoi <- as.character(aoi_col)
  out <- mutate(df_summarized,
               AOI = aoi,
               Elog = log( (SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5) ),
               Weights = 1 / ( ( 1 / (SamplesInAOI + .5) ) / ( 1 / (SamplesTotal - SamplesInAOI +.5) ) ),
               Prop = SamplesInAOI / SamplesTotal,
               ArcSin = asin( sqrt( Prop ))
  )
  out <- ungroup(out)
}


# Friendly Dplyr Verbs ----------------------------------------------------------------------------------
# dplyr verbs remove custom classes from dataframe, so a custom method needs to be written to avoid this

mutate_.time_sequence_data <- mutate_.time_window_data <- mutate_.bin_analysis <- mutate_.bootstrapped_data <- mutate_.time_cluster_data <- mutate_.bootstrap_analysis <- mutate_.onset_data <- function(data, ...) {

  # remove class names (avoid infinite recursion):
  potential_classes <- c('time_sequence_data', 'time_window_data', 'onset_data', 'bootstrapped_data', 'bootstrap_analysis', "time_cluster_data", 'bin_analysis')
  temp_remove <- class(data)[ class(data) %in% potential_classes]
  class(data) <- class(data)[!class(data) %in% potential_classes]
  temp_attr <- attr(data, "eyetrackingR") # also attributes

  out <- mutate_(data, ...)

  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr

  return(out)
}

group_by_.time_sequence_data <- group_by_.time_window_data <- group_by_.bin_analysis <- group_by_.bootstrapped_data <- group_by_.time_cluster_data <- group_by_.bootstrap_analysis <- group_by_.onset_data <- function(data, ...) {

  # remove class names (avoid infinite recursion):
  potential_classes <- c('time_sequence_data', 'time_window_data', 'onset_data', 'bootstrapped_data', 'bootstrap_analysis', "time_cluster_data", 'bin_analysis')
  temp_remove <- class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr <- attr(data, "eyetrackingR") # also attributes

  out <- group_by_(data, ...)

  # reapply class/attributes
  class(out) <- c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr

  return(out)
}

filter_.time_sequence_data <- filter_.time_window_data <- filter_.bin_analysis <- filter_.bootstrapped_data <- filter_.time_cluster_data <- filter_.bootstrap_analysis <- filter_.onset_data <- function(data, ...) {

  # remove class names (avoid infinite recursion):
  potential_classes <- c('time_sequence_data', 'time_window_data', 'onset_data', 'bootstrapped_data', 'bootstrap_analysis', "time_cluster_data", 'bin_analysis')
  temp_remove <- class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr <- attr(data, "eyetrackingR") # also attributes

  out <- filter_(data, ...)

  # reapply class/attributes
  class(out) <- c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr

  return(out)
}

left_join.time_sequence_data <- left_join.time_window_data <- left_join.bin_analysis <- left_join.time_cluster_data <- left_join.bootstrapped_data <- left_join.bootstrap_analysis <- left_join.onset_data <- function(x, y, by = NULL, copy = FALSE, ...) {

  # remove class names (avoid infinite recursion):
  potential_classes <- c('time_sequence_data', 'time_window_data', 'onset_data', 'bootstrapped_data', 'bootstrap_analysis', "time_cluster_data", 'bin_analysis')
  temp_remove <- class(x)[ class(x) %in% potential_classes]
  class(x) <- class(x)[!class(x) %in% potential_classes]
  temp_attr <- attr(x, "eyetrackingR") # also attributes

  out <- left_join(x=x, y=y, by = by, copy = copy, ...)

  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr

  return(out)
}
