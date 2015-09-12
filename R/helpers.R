#' Prevent accidental plotting of data.frames whose class has been removed.
#'
#' EyetrackingR adds a plot method to most dataframes you can create with it.
#' Sometimes, the class of the dataframe can be removed, so that plot will
#' not have the expected result. This ensures a warning is issued to the user.
#'
#' @param data
#' @export
#' @return NULL
plot.data.frame <- function(data) {
  stop(
    "Cannot plot this data. Either no plotting method exists for this data, or the class of this data, which specifies ",
    "what type of data it is, has been removed. This can happen by using functions that transform the data significantly, ",
    "such as dplyr's 'summarize' and 'select'."
  )
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
.make_proportion_looking_summary <-
  function(data, groups, aoi_col) {
    # Group, summarize Samples
    df_grouped <- group_by_(data, .dots = groups)
    df_summarized <- summarize_(df_grouped,
                                .dots = list(
                                  SamplesInAOI = interp( ~ sum(AOI_COL, na.rm = TRUE), AOI_COL = aoi_col),
                                  SamplesTotal = interp( ~ sum(!is.na(AOI_COL)), AOI_COL = aoi_col) # ignore all NAs
                                ))

    # Calculate Proportion, Elog, etc.
    aoi <- as.character(aoi_col)
    out <- mutate(
      df_summarized,
      AOI = aoi,
      Elog = log((SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5)),
      Weights = 1 / ((1 / (SamplesInAOI + .5)) / (1 / (
        SamplesTotal - SamplesInAOI + .5
      ))),
      Prop = SamplesInAOI / SamplesTotal,
      ArcSin = asin(sqrt(Prop))
    )
    out <- ungroup(out)
  }


# Friendly Dplyr Verbs: Mutate ----------------------------------------------------------------------------------
#' EyetrackingR friendly mutate
#' @describeIn mutate_
#' @param data A dataframe
#' @export
#' @return A dataframe
mutate_.time_sequence_data <- function(data, ...) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 'boot_splines_analysis', "time_cluster_data", 'bin_analysis'
    )
  temp_remove <- class(data)[class(data) %in% potential_classes]
  class(data) <- class(data)[!class(data) %in% potential_classes]
  temp_attr <- attr(data, "eyetrackingR") # also attributes

  out <- mutate_(data, ...)

  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr

  return(out)
}

#' @describeIn mutate_
#' @export
mutate_.time_window_data <- mutate_.time_sequence_data

#' @describeIn mutate_
#' @export
mutate_.bin_analysis <- mutate_.time_sequence_data

#' @describeIn mutate_
#' @export
mutate_.boot_splines_data <- mutate_.time_sequence_data

#' @describeIn mutate_
#' @export
mutate_.time_cluster_data <- mutate_.time_sequence_data

#' @describeIn mutate_
#' @export
mutate_.boot_splines_analysis <- mutate_.time_sequence_data

#' @describeIn mutate_
#' @export
mutate_.onset_data <- mutate_.time_sequence_data


# Friendly Dplyr Verbs: GroupBy ----------------------------------------------------------------------------------

#' EyetrackingR friendly group_by
#' @describeIn group_by_
#' @param data A dataframe
#' @export
#' @return A dataframe
group_by_.time_sequence_data <- function(data, ...) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 'boot_splines_analysis', "time_cluster_data", 'bin_analysis'
    )
  temp_remove <- class(data)[class(data) %in% potential_classes]
  class(data) <- class(data)[!class(data) %in% potential_classes]
  temp_attr <- attr(data, "eyetrackingR") # also attributes

  out <- group_by_(data, ...)

  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr

  return(out)
}

#' @describeIn group_by_
#' @export
group_by_.time_window_data <- group_by_.time_sequence_data

#' @describeIn group_by_
#' @export
group_by_.bin_analysis <- group_by_.time_sequence_data

#' @describeIn group_by_
#' @export
group_by_.boot_splines_data <- group_by_.time_sequence_data

#' @describeIn group_by_
#' @export
group_by_.time_cluster_data <- group_by_.time_sequence_data

#' @describeIn group_by_
#' @export
group_by_.boot_splines_analysis <- group_by_.time_sequence_data

#' @describeIn group_by_
#' @export
group_by_.onset_data <- group_by_.time_sequence_data


# Friendly Dplyr Verbs: Filter ----------------------------------------------------------------------------------

#' EyetrackingR friendly filter
#' @describeIn filter_
#' @param data A dataframe
#' @export
#' @return A dataframe
filter_.time_sequence_data <- function(data, ...) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 'boot_splines_analysis', "time_cluster_data", 'bin_analysis'
    )
  temp_remove <- class(data)[class(data) %in% potential_classes]
  class(data) <- class(data)[!class(data) %in% potential_classes]
  temp_attr <- attr(data, "eyetrackingR") # also attributes

  out <- filter_(data, ...)

  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr

  return(out)
}

#' @describeIn filter_
#' @export
filter_.time_window_data <- filter_.time_sequence_data

#' @describeIn filter_
#' @export
filter_.bin_analysis <- filter_.time_sequence_data

#' @describeIn filter_
#' @export
filter_.boot_splines_data <- filter_.time_sequence_data

#' @describeIn filter_
#' @export
filter_.time_cluster_data <- filter_.time_sequence_data

#' @describeIn filter_
#' @export
filter_.boot_splines_analysis <- filter_.time_sequence_data

#' @describeIn filter_
#' @export
filter_.onset_data <- filter_.time_sequence_data

# Friendly Dplyr Verbs: Left-Join ----------------------------------------------------------------------------------

#' EyetrackingR friendly Left-Join
#' @describeIn left_join
#' @param x Left dataframe
#' @param y Right dataframe
#' @param by a character vector of variables to join by
#' @param copy Copy to same src? Default FALSE
#' @param ... other parameters passed onto methods
#' @export
#' @return A dataframe
left_join.time_sequence_data <-
  function(x, y, by = NULL, copy = FALSE, ...) {
    # remove class names (avoid infinite recursion):
    potential_classes <-
      c(
        'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 'boot_splines_analysis', "time_cluster_data", 'bin_analysis'
      )
    temp_remove <- class(x)[class(x) %in% potential_classes]
    class(x) <- class(x)[!class(x) %in% potential_classes]
    temp_attr <- attr(x, "eyetrackingR") # also attributes

    out <- left_join(
      x = x, y = y, by = by, copy = copy, ...
    )

    # reapply class/attributes
    class(out) = c(temp_remove, class(out))
    attr(out, "eyetrackingR") = temp_attr

    return(out)
  }

#' @describeIn left_join
#' @export
left_join.time_window_data <- left_join.time_sequence_data

#' @describeIn left_join
#' @export
left_join.bin_analysis <- left_join.time_sequence_data

#' @describeIn left_join
#' @export
left_join.boot_splines_data <- left_join.time_sequence_data

#' @describeIn left_join
#' @export
left_join.time_cluster_data <- left_join.time_sequence_data

#' @describeIn left_join
#' @export
left_join.boot_splines_analysis <- left_join.time_sequence_data

#' @describeIn left_join
#' @export
left_join.onset_data <- left_join.time_sequence_data
