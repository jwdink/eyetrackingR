#' Prevent accidental plotting of data.frames whose class has been removed.
#'
#' EyetrackingR adds a plot method to most dataframes you can create with it.
#' Sometimes, the class of the dataframe can be removed, so that plot will
#' not have the expected result. This ensures a warning is issued to the user.
#'
#' @param data
#' @export
#' @return NULL
plot.data.frame <- function(data, ...) {
  stop(
    "Cannot plot this data. Either no plotting method exists for this data, or the class of this data, which specifies ",
    "what type of data it is, has been removed. This can happen by using functions that transform the data significantly, ",
    "such as dplyr's 'summarize' and 'select'."
  )
}

#' .label_consecutive()
#'
#' A helper function to label/enumerate runs of TRUEs in a logical vector, with NA for falses.
#'
#' @param vec Logical
#' @return A numeric vector
.label_consecutive <- function(vec) {
  vec = c(0,vec)
  vec[is.na(vec)] = 0
  out = c(cumsum(diff(vec)==1))
  vec = vec[-1]
  out[!vec] = NA
  out
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
    
    .logit_adj <- function(prop) {
      non_zero_ind <- which(prop!=0)
      if (length(non_zero_ind) < 1) return(NA)
      smallest <- min(prop[non_zero_ind], na.rm=TRUE)
      prop_adj <- prop
      prop_adj <- ifelse(prop_adj == 1, 1 - smallest/2, prop_adj)
      prop_adj <- ifelse(prop_adj == 0, smallest/2, prop_adj)
      return( log(prop_adj / (1-prop_adj)) )
    }
    
    # Group, summarize Samples
    df_grouped <- group_by_(data, .dots = groups)
    df_summarized <- summarize_(df_grouped,
                                .dots = list(
                                  SamplesInAOI = interp( ~ sum(AOI_COL, na.rm = TRUE), AOI_COL = aoi_col),
                                  SamplesTotal = interp( ~ sum(!is.na(AOI_COL)), AOI_COL = aoi_col) # ignore all NAs
                                ))
    
    # Calculate Proportion, Elog, etc.
    aoi <- as.character(aoi_col)
    out <- ungroup(df_summarized)
    out <- mutate(out,
                  AOI = aoi,
                  Elog = log((SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5)),
                  Weights = 1 / ( (1 / (SamplesInAOI + .5)) / (1 / (SamplesTotal - SamplesInAOI + .5)) ),
                  Prop = SamplesInAOI / SamplesTotal,
                  LogitAdjusted = .logit_adj(Prop),
                  ArcSin = asin(sqrt(Prop))
    )
    out
  }

#' Run a function, return result, errors, and warnings
#'
#' This function is from \link{http://stackoverflow.com/a/4952908/3291744}
#'
#' Thanks to user Martin Morgan for this ingenious solution.
#'
#' @param function
#' @return new version of that function
.make_function_fail_informatively <- function(fun)
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    list(res, warn=warn, err=err)
  }


# EyetrackingR-Friendly-Subset ----------------------------------------------------------------------------------

#' EyetrackingR friendly subset
#' @describeIn subset
#' @param data A dataframe
#' @export
#' @return A dataframe
subset.time_sequence_data <- function(x, ...) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 'boot_splines_analysis', "time_cluster_data", 'bin_analysis'
    )
  temp_remove <- class(x)[class(x) %in% potential_classes]
  class(x) <- class(x)[!class(x) %in% potential_classes]
  temp_attr <- attr(x, "eyetrackingR") # also attributes
  
  out <- subset(x, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

#' @describeIn subset
#' @export
subset.time_window_data <- subset.time_sequence_data

#' @describeIn subset
#' @export
subset.bin_analysis <- subset.time_sequence_data

#' @describeIn subset
#' @export
subset.boot_splines_data <- subset.time_sequence_data

#' @describeIn subset
#' @export
subset.time_cluster_data <- subset.time_sequence_data

#' @describeIn subset
#' @export
subset.boot_splines_analysis <- subset.time_sequence_data

#' @describeIn subset
#' @export
subset.onset_data <- subset.time_sequence_data

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



## Should NOT be necessary 
## (only needed for tbl_dfs, and if your eyetrackingR object has tbl_df class then this won't get caught anyways)
# EyetrackingR friendly extract
# #' @describeIn Extract
# #' @param data A dataframe
# #' @export
# #' @return A dataframe
# 
# `[.time_sequence_data` <- function(data, i, j, ...) {
#   # remove class names (avoid infinite recursion):
#   potential_classes <-
#     c(
#       'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 'boot_splines_analysis', "time_cluster_data", 'bin_analysis'
#     )
#   temp_remove <- class(data)[class(data) %in% potential_classes]
#   class(data) <- class(data)[!class(data) %in% potential_classes]
#   temp_attr <- attr(data, "eyetrackingR") # also attributes
#   
#   out <- data[i = i, j = j,... = ...]
#   
#   # reapply class/attributes
#   class(out) = c(temp_remove, class(out))
#   attr(out, "eyetrackingR") = temp_attr
#   
#   return(out)
# }
# 
# #' @describeIn Extract
# #' @export
# `[.time_window_data` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.bin_analysis` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.boot_splines_data` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.time_cluster_data` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.boot_splines_analysis` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.onset_data` <- `[.time_sequence_data`