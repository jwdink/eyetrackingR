#' Make onset-contingent data.
#'
#' Divide trials into which AOI they started on; augment with column indicating switch away from that AOI
#'
#' @param data            The original (verified) data
#' @param data_options
#' @param onset_time        When to check for participants' "starting" AOI?
#' @param fixation_window_length       Which AOI is currently being fixated is determined by taking a rolling
#'   average. This is the width of window for rolling average.
#' @param target_aoi      Which AOI is the target that should be switched *to*
#' @param distractor_aoi  Which AOI is the distractor that should be switched *from* (default = !target_aoi)
#' @export
#' @return Original dataframe augmented with column indicating switch away from target AOI

make_onset_data <- function(data, data_options, onset_time, fixation_window_length, target_aoi, distractor_aoi = NULL) {
  ## Helper Function:
  na_replace_rollmean <- function(col) {
    col <- ifelse(is.na(col), 0, col)
    zoo::rollmean(col, k = fixation_window_length_rows, partial=TRUE, fill= NA, align="left")
  }

  ## Prelims:
  if (is.null(distractor_aoi)) {
    distractor_aoi <- paste0("Not_", target_aoi)
    data[[distractor_aoi]] <- !data[[target_aoi]]
  }
  time_col <- as.name(data_options$time_column)

  ## Translate TimeWindow units from time to number of rows (for rolling mean):
  df_time_per_row <- group_by_(data, .dots = c(data_options$participant_column, data_options$trial_column) )
  df_time_per_row <- summarize_(df_time_per_row,
                               .dots = list(TimePerRow = interp(~mean(diff(TIME_COL)), TIME_COL = time_col)
                                            ))
  time_per_row <- round(mean(df_time_per_row[["TimePerRow"]]))
  fixation_window_length_rows <- fixation_window_length / time_per_row

  ## Determine First AOI, Assign Switch Value for each timepoint

  # Group by Ppt*Trial:
  df_grouped <- group_by_(data, .dots = list(data_options$participant_column, data_options$trial_column) )

  # Create a rolling-average of 'inside-aoi' logical for target and distractor, to give a smoother estimate of fixations
  df_smoothed <- mutate_(df_grouped,
                        .dots = list(.Target    = interp(~na_replace_rollmean(TARGET_AOI), TARGET_AOI = as.name(target_aoi)),
                                     .Distractor= interp(~na_replace_rollmean(DISTRACTOR_AOI), DISTRACTOR_AOI = as.name(distractor_aoi)),
                                     .Time      = interp(~TIME_COL, TIME_COL = time_col)
                                     ))

  # For any trials where no data for onset timepoint is available, find the closest timepoint.
  # Calculate FirstAOI
  df_first_aoi <- mutate(df_smoothed,
                        .ClosestTime = ifelse(length(which.min(abs(.Time - onset_time)))==1, .Time[which.min(abs(.Time - onset_time))], NA),
                        FirstAOI     = ifelse(.Target[.Time==.ClosestTime] > .Distractor[.Time==.ClosestTime], target_aoi, distractor_aoi)
  )
  df_first_aoi <- ungroup(df_first_aoi)

  # If closest timepoint was too far away from onset window, record FirstAOI as unknown
  # Create a column specifying whether they have switched away from FirstAOI
  out <- mutate(df_first_aoi,
         FirstAOI  = ifelse(abs(.ClosestTime-onset_time) > fixation_window_length, NA, FirstAOI),
         WhichAOI  = ifelse(.Target > .Distractor, target_aoi, distractor_aoi),
         SwitchAOI = FirstAOI != WhichAOI)
  out <- select(out, -.Target, -.Distractor, -.Time, -.ClosestTime)

  # Assign class information:
  out <- as.data.frame(out)
  class(out)  <- c('onset_data', class(out))
  attr(out, 'eyetrackingR')  <- list(data_options = data_options,
                                     onset_contingent = list(
                                       distractor_aoi = distractor_aoi,
                                       target_aoi = target_aoi,
                                       onset_time = onset_time,
                                       fixation_window_length = fixation_window_length))

  return(out)
}

#' Summarize data into time-to-switch from initial AOI.
#'
#' Take trials split by initial-AOI, and determine how quickly participants switch away from that AOI
#' @export
make_switch_data <- function(data, ...) {
  UseMethod("make_switch_data")
}
#' @describeIn make_switch_data
#'
#' @param data               The output of \code{make_onset_data}
#' @param predictor_columns  Variables/covariates of interest when analyzing time-to-switch
#' @param summarize_by       Should the data be summarized along, e.g., participants, items, etc.? If so, give
#'   column name(s) here. If left blank, will leave trials distinct. The former is needed for more traditional
#'   analyses (t.tests, ANOVAs), while the latter is preferable for mixed-effects models (lmer)
#' @export
#' @return A dataframe indicating initial AOI and time-to-switch from that AOI for each trial/subject/item/etc.
make_switch_data.onset_data <- function(data, predictor_columns=NULL, summarize_by = NULL) {

  data_options = attr(data, "eyetrackingR")$data_options

  if (is.null(summarize_by)) {
    summarize_by <- c(data_options$participant_column,
                     data_options$trial_column,
                     data_options$item_columns)
  }

  time_col <- as.name(data_options$time_column)

  df_cleaned <- filter(data, !is.na(FirstAOI))
  df_grouped <- group_by_(data,
                         .dots = c(summarize_by,
                                   "FirstAOI",
                                   predictor_columns))
  df_summarized <- summarize_(df_grouped,
                            .dots = list(FirstSwitch = interp(~TIME_COL[first(which(SwitchAOI), order_by= TIME_COL)], TIME_COL = time_col)
                                         ))

  df_summarized <- as.data.frame(df_summarized)
  class(df_summarized) <- c('switch_data', class(df_summarized))
  attr(df_summarized, "eyetrackingR") <- list(data_options = data_options)

  return(df_summarized)
}

#' Plot onset-contingent data
#'
#' Divide trials into which AOI participants started on; plot proportion looking away from that AOI.
#'
#' @param data The output of the \code{make_onset_data} function
#' @param predictor_columns Column(s) by which to facet the data. Does NOT perform median split if numeric.
#' @export
#' @return A ggplot object

plot.onset_data <- function(data, predictor_columns=NULL) {

  ## Prelims:
  attrs <- attr(data, "eyetrackingR")
  data_options <- attrs$data_options
  if (is.null(data_options)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  onset_attr <- attrs$onset_contingent
  if (is.null(onset_attr)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later

  # set smoothing_window_size based on fixation_window_length in attr's
  smoothing_window_size <- onset_attr$fixation_window_length / 5

  # clean out unknown first AOIs:
  df_clean <- data[ !is.na(data[["FirstAOI"]]) , ]
  for (predictor_col in predictor_columns) {
    if (! identical(TRUE, predictor_col %in% colnames(df_clean)) ) stop("The column ", predictor_col, " is not in the dataset.")
    df_clean <- df_clean[ !is.na(df_clean[[predictor_col]]) , ]
  }

  # summarize by time bin:
  df_clean[[".Time"]] <- floor(df_clean[[data_options$time_column]] / smoothing_window_size) * smoothing_window_size
  df_grouped <- group_by_(df_clean,
                         .dots = c(predictor_columns, data_options$participant_column, ".Time", "FirstAOI", onset_attr$distractor_aoi, onset_attr$target_aoi))
  df_smoothed <- summarize(df_grouped, SwitchAOI = mean(SwitchAOI, na.rm=TRUE))

  # collapse into lines for graphing:
  df_summarized <- group_by_(df_smoothed, .dots = c(".Time", "FirstAOI", predictor_columns) )
  df_summarized <- summarize(df_summarized, SwitchAOI = mean(SwitchAOI, na.rm=TRUE))

  # compute grayed area:
  df_graph <- group_by_(df_summarized, .dots = c(".Time", predictor_columns) )
  df_graph <- mutate(df_graph,
                    Max= max(SwitchAOI),
                    Min= min(SwitchAOI),
                    Top= ifelse(length(which(FirstAOI==onset_attr$distractor_aoi)), SwitchAOI[FirstAOI==onset_attr$distractor_aoi], 0)
                    )
  df_graph$Max <- with(df_graph, ifelse(Max==Top, Max, NA))
  df_graph$Min <- with(df_graph, ifelse(Max==Top, Min, NA))

  ## Graph:
  if (is.null(predictor_columns)) {
    color_factor <- NULL
  } else {
    color_factor <- predictor_columns[1]
  }
  g <- ggplot(df_graph, aes_string(x = ".Time", y = "SwitchAOI",
                                  group = "FirstAOI",
                                  color = color_factor)) +
    geom_line(size=1.5, aes(linetype=FirstAOI)) +
    geom_ribbon(aes(ymin= Min, ymax= Max), fill= "gray", alpha= .2, colour= NA) +
    coord_cartesian(xlim=c(onset_attr$onset_time, max(df_graph$.Time) )) +
    ylab("Proportion Switch Looking") +
    xlab("Time")

  ## Add Facets for Conditions:
  if (length(predictor_columns)>1) return(g+facet_grid(as.formula(paste(predictor_columns, collapse="~"))))
  if (length(predictor_columns)>0) return(g+facet_grid(as.formula(paste(predictor_columns, "~ ."))))
  return(g)

}

#' Plot mean switch-from-initial-AOI times.
#'
#' Boxplot of mean switch time aggregated by subjects within each FirstAOI, potentially faceted by predictor_columns.
#'
#' @param data The output of the \code{make_switch_data} function
#' @param predictor_columns Column(s) by which to facet the data. Does NOT perform median split if numeric.
#' @export
#' @return A ggplot object

plot.switch_data <- function(data, predictor_columns=NULL) {

  ## Prelims:
  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  if (length(predictor_columns) > 2) {
    stop("Maximum two predictor columns.")
  }

  ## Prepare for Graphing:
  data <- filter(data, !is.na(FirstAOI))
  df_grouped <- group_by_(data, .dots = c(data_options$participant_column, predictor_columns, "FirstAOI"))
  df_summarized <- summarize(df_grouped, MeanFirstSwitch = mean(FirstSwitch))

  ## Graph:
  if (is.null(predictor_columns)) {
    color_factor <- NULL
  } else {
    color_factor <- predictor_columns[1]
  }

  g <- ggplot(df_summarized, aes_string(x = "FirstAOI", y = "MeanFirstSwitch",
                             color = color_factor)) +
    geom_boxplot() +
    geom_point(position = position_jitter(.1)) +
    coord_flip() +
    ylab("Mean Switch Time") +
    xlab("Onset AOI")

  ## Add Facets for Conditions:
  if (length(predictor_columns)>1) return(g+facet_grid(as.formula(paste(predictor_columns, collapse="~"))))
  if (length(predictor_columns)>0) return(g+facet_grid(as.formula(paste(predictor_columns, "~ ."))))

  return(g)

}