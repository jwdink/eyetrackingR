# Loading/Cleaning/Describing Data ------------------------------------------------------------------------

#' Set data options.
#'
#' Create a list which describes the important aspects of your data, to be used by most other functions in
#' this package.
#'
#' @param participant_column Column name for participant identifier
#' @param trackloss_column   Column name indicating trackloss
#' @param time_column        Column name indicating time
#' @param trial_column       Column name indicating trial identifier
#' @param item_columns       Column names indicating items (can be same as trial_column)
#' @param aoi_columns        Names of AOIs
#' @return list of configuration options

set_data_options <- function(participant_column,
                             trackloss_column,
                             time_column,
                             trial_column,
                             item_columns = NULL,
                             aoi_columns) {
  list(participant_column = participant_column,
       trackloss_column = trackloss_column,
       time_column = time_column,
       trial_column = trial_column,
       item_columns = item_columns,
       aoi_columns = aoi_columns)
}


#' Verify the dataset
#'
#' Use data_options to verify that the columns in your dataset are of the correct type
#'
#' @param data Your data
#' @param data_options Created by \code{set_data_options}
#' @return Dataset with verified column types.

verify_dataset <- function(data, data_options) {
  out <- data

  if (!all(data_options$aoi_columns %in% colnames(data))) {
    stop("Not all of the AOI columns specified in data_options are in the data.")
  }

  as.numeric2 <- function(x) as.numeric(as.character(x))
  check_then_convert <- function(x, checkfunc, convertfunc, colname) {
    if (!checkfunc(x)) {
      message("Converting ", colname, " to proper type.")
      convertfunc(x)
    } else {
      x
    }
  }
  col_type_converter <- list(participant_column = function(x) check_then_convert(x, is.factor, as.factor, "Participants"),
                             time_column = function(x) check_then_convert(x, is.numeric, as.numeric2, "Time"),
                             trial_column = function(x) check_then_convert(x, is.factor, as.factor, "Trial"),
                             trackloss_column = function(x) check_then_convert(x, is.logical, as.logical, "Trackloss"),
                             item_columns = function(x) check_then_convert(x, is.factor, as.factor, "Item"),
                             aoi_columns = function(x) check_then_convert(x, is.logical, as.logical, "AOI"))

  for (col in names(col_type_converter)) {
    for (i in seq_along(data_options[[col]])) {
      if (is.null(out[[data_options[[col]][i]]]))
        stop("Data are missing: ", col)
      out[[data_options[[col]][i]]] <- col_type_converter[[col]](out[[data_options[[col]][i]]])
    }
  }

  return(out)
}

#' Extract a subset of the dataset within a time-window in each trial.
#'
#' Extract a subset of the dataset, where each trial falls inside a time-window.
#' Time-window can either be specifed by a number (for a timestamp across all trials) or
#' by a column which picks out a timestamp for each participant/trial
#'
#' @param data
#' @param data_options
#' @param window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param window_end Number (for timestamp) or character (for column that specifies timestamp)
#' @param rezero Should the beginning of the window be considered the zero point of the timestamp?
#'  Default TRUE when window_start is column, FALSE when window_start is number
#' @return Subsetted data

subset_by_window <- function(data, data_options, window_start = -Inf, window_end = Inf, rezero = NULL) {

  # Prelims:
  time_col <- as.name(data_options$time_column)

  # Window Start:
  if (is.character(window_start)) {
    data$.WindowStart <- data[[window_start]]
    if (is.null(rezero))
      rezero <- TRUE
  } else {
    data$.WindowStart <- window_start
    if (is.null(rezero))
      rezero <- FALSE
  }

  # Window End:
  if (is.character(window_end)) {
    data$.WindowEnd <- data[[window_end]]
  } else {
    data$.WindowEnd <- window_end
  }

  # Subset
  df_subsetted <- filter_(.data = data,
                          .dots = list(interp(~TIME_COL >= .WindowStart & TIME_COL <= .WindowEnd, TIME_COL = time_col)))

  # Rezero
  if (rezero) {
    df_grouped <- group_by_(.data = df_subsetted,
                            .dots = list(data_options$participant_column, data_options$trial_column
                            ))
    df_rezeroed <- mutate_(.data = df_grouped,
                           .dots = list(.NewTimeStamp = interp(~TIME_COL - .WindowStart, TIME_COL = time_col)
                           ))
    out <- ungroup(df_rezeroed)
    out[[data_options$time_column]] <- out[[".NewTimeStamp"]]
    out[[".NewTimeStamp"]] <- NULL
  } else {
    out <- df_subsetted
  }

  out[[".WindowStart"]] <- NULL
  out[[".WindowEnd"]] <- NULL

  out
}

#' Analyze trackloss.
#'
#' Get information on trackloss in your data. This can be for the entire dataset, or for a specific time
#' window within a trial
#'
#' @param data
#' @param data_options
#' @param window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param window_end Number (for timestamp) or character (for column that specifies timestamp)
#' @return A dataframe describing trackloss by-trial and by-participant

trackloss_analysis <- function(data, data_options, window_start = -Inf, window_end = Inf) {

  trackloss_col <- as.name(data_options$trackloss_column)

  # Filter by Time-Window:
  df_subsetted <- subset_by_window(data, data_options, window_start, window_end)

  # Get Trackloss-by-Trial:
  df_grouped_trial <- group_by_(df_subsetted, .dots = list(data_options$participant_column, data_options$trial_column))
  df_trackloss_by_trial <- mutate_(df_grouped_trial,
                                   .dots = list(SumTracklossForTrial = interp(~sum(TRACKLOSS_COL, na.rm = TRUE), TRACKLOSS_COL = trackloss_col),
                                                TotalTrialLength = interp(~length(TRACKLOSS_COL), TRACKLOSS_COL = trackloss_col),
                                                TracklossForTrial = interp(~SumTracklossForTrial/TotalTrialLength)
                                   ))

  # Get Trackloss-by-Participant:
  df_grouped_ppt <- group_by_(df_trackloss_by_trial, .dots = list(data_options$participant_column))
  df_trackloss_by_ppt <- mutate_(df_grouped_ppt,
                                 .dots = list(SumTracklossForParticipant = interp(~sum(TRACKLOSS_COL, na.rm = TRUE), TRACKLOSS_COL = trackloss_col),
                                              TotalParticipantLength = interp(~length(TRACKLOSS_COL), TRACKLOSS_COL = trackloss_col),
                                              TracklossForParticipant = interp(~SumTracklossForParticipant/TotalParticipantLength)))

  # Get Z-Scores:
  df_grouped <- group_by_(df_trackloss_by_ppt, .dots = list(data_options$participant_column, data_options$trial_column))
  df_summarized <- summarize(df_grouped,
                             Samples = mean(TotalTrialLength, na.rm = TRUE),
                             TracklossSamples = mean(SumTracklossForTrial, na.rm = TRUE),
                             TracklossForTrial = mean(TracklossForTrial, na.rm = TRUE),
                             TracklossForParticipant = mean(TracklossForParticipant, na.rm = TRUE))
  df_summarized <- ungroup(df_summarized)

  return(df_summarized)
}


#' Clean data by removing high-trackloss trials/subjects.
#'
#' Remove trials/participants with too much trackloss, with a customizable threshold. This can be for the
#' entire dataset, or for a specific time window within a trial.
#'
#' @param data
#' @param data_options
#' @param participant_prop_thresh Maximum proportion of trackloss for participants
#' @param trial_prop_thresh Maximum proportion of trackloss for trials
#' @param window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param window_end Number (for timestamp) or character (for column that specifies timestamp)
#' @return Cleaned data

clean_by_trackloss <- function(data, data_options,
                              participant_prop_thresh = 1,
                              trial_prop_thresh = 1,
                              window_start = -Inf, window_end = Inf) {

  # Helpful Column:
  data$.TrialID <- paste(data[[data_options$participant_col]], data[[data_options$trial_col]], sep = "_")

  # Trackloss Analysis:
  message("Performing Trackloss Analysis...")
  tl <- trackloss_analysis(data, data_options, window_start, window_end)

  # Bad Trials:
  if (trial_prop_thresh < 1) {
    message("Will exclude trials whose trackloss proportion is greater than : ", trial_prop_thresh)
    exclude_trials_props <- paste(tl$Participant[tl$TracklossForTrial > trial_prop_thresh],
                                 tl$Trial[tl$TracklossForTrial > trial_prop_thresh],
                                 sep="_")
    message(paste("\t...removed ", length(exclude_trials_props), " trials."))
  } else {
    exclude_trials_props <- c()
  }

  # Bad Participants
  part_vec <- data[[data_options$participant_col]]
  if (participant_prop_thresh < 1) {
    message("Will exclude participants whose trackloss proportion is greater than : ", participant_prop_thresh)
    exclude_ppts_prop <- unique(tl$Participant[tl$TracklossForParticipant > participant_prop_thresh])
    message(paste("\t...removed ", length(exclude_ppts_prop), " participants."))
  } else {
    exclude_ppts_prop <- c()
  }

  exclude_trials <- c(exclude_trials_props, unique( data$.TrialID[part_vec %in% exclude_ppts_prop] ))

  # Remove:
  data_clean <- filter(data, ! .TrialID %in% exclude_trials)
  data_clean$.TrialID <- NULL

  return(data_clean)

}


#' Treat gaze data outside of all AOIs as trackloss.
#'
#' Should gaze outside of any AOI be considered trackloss? This function sets trackloss to TRUE for any
#' samples that were not inside an AOI
#'
#' @param data
#' @param data_options
#'
#' @return Data where non-AOI looks are now considered trackloss.

convert_non_aoi_to_trackloss <- function(data, data_options) {

  # Create version of AOIs with no NAs:
  .narepl <- function(x) ifelse(is.na(x), 0, x)
  data_no_na <- mutate_each_(data, funs(.narepl), vars = sapply(data_options$aoi_columns, as.name))

  # Find all rows which have no AOIs in them:
  data_no_na[[".AOISum"]] <- 0
  for (aoi in data_options$aoi_columns) {
    data_no_na[[".AOISum"]] <- data_no_na[[".AOISum"]] + data_no_na[[aoi]]
  }

  # Set these rows as trackloss:
  data[[data_options$trackloss_column]] <- ifelse(data_no_na[[".AOISum"]] == 0, TRUE, data_no_na[[data_options$trackloss_column]])

  return(data)
}

#' Keep data with trackloss in dataset.
#'
#' Converts data so that, in all subsequent proportion-looking calculations, proportion_looking_at_aoi =
#' time_looking_at_aoi / time_possible. Trackloss is *not* considered missing data, but instead is counted as
#' time *not* spent looking at each AOI.
#'
#' @param data
#' @param data_options
#' @return Data with trackloss preserved

keep_trackloss <- function(data, data_options) {
  out <- data

  # Create version of AOIs which = FALSE for any trackloss samples:
  for (aoi in data_options$aoi_columns) {
    out[[aoi]] <- ifelse(out[[data_options$trackloss_column]]==1, FALSE, out[[aoi]] )
  }

  # If there is still missing data for AOIs, warn that it's going to be ignored:
  for (aoi in data_options$aoi_columns) {
    if (any(is.na(data_with_trackloss[[aoi]]))) {
      warning("NAs found for non-trackloss samples, in '", aoi,
              "'' column. These samples will be interpreted as being outside of the ", aoi, " AOI.")
    }
  }

  # Replace any Lingering Missing AOI Data:
  for (aoi in data_options$aoi_columns) {
    out[[aoi]] <- ifelse(is.na(out[[aoi]]), FALSE, out[[aoi]] )
  }
  return(out)
}

#' Remove data with trackloss from dataset.
#'
#' Converts data so that, in all subsequent proportion-looking calculations, proportion_looking_at_aoi =
#' time_looking_at_aoi / time_looking. Trackloss *is* considered missing data, and time spent not looking at a
#' given AOI must have been spent looking at another AOI
#'
#' @param data
#' @param data_options
#' @param delete_rows (default: FALSE) Should rows with trackloss be deleted, or simply set to NA?
#'
#' @return Data with trackloss removed
remove_trackloss <- function(data, data_options, delete_rows = FALSE) {
  out <- data

  if (delete_rows == TRUE) {

    # We want to only remove rows that are positively identified as trackloss, so we replace any trackloss=NA with trackloss=FALSE
    out[[".TracklossBoolean"]] <- ifelse(is.na(out[[data_options$trackloss_column]]), FALSE, out[[data_options$trackloss_column]])

    # Remove all rows with Trackloss:
    out <- filter(out, .TracklossBoolean == 0)
    out <- out[, !(colnames(out) %in% c('.TracklossBoolean'))]

  } else {
    # Set Looking-at-AOI to NA for any samples where there is Trackloss:

    # TODO: fix bug that removes all AOI looks when delete_rows == FALSE (JD: not understanding this bug)

    for (aoi in data_options$aoi_columns) {
      out[[aoi]] <- ifelse(data[[data_options$trackloss_column]]==1, NA, out[[aoi]] )
    }
  }

  out
}

#' Describe dataset
#'
#' Returns descriptive statistics about a column of choice. A simple wrapper around dplyr:group_by/
#' dplyr::summarize that allows a quick glance at the data.
#'
#' @param data
#' @param data_options
#' @param describe_column The column to return descriptive statistics about.
#' @param group_columns Any columns to group by when calculating descriptive statistics (e.g., participants,
#'  conditions, etc.)
#' @return A dataframe giving descriptive statistics for the \code{describe_column}, including mean, SD, var,
#' min, max, and number of trials

describe_data <- function(data, data_options = NULL, describe_column, group_columns) {

  # Data options:
  if (is.null(data_options)) data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) stop("Please supply data_options.")

  # Build Summarize Expression
  summarize_expr <- list(Mean = interp( ~mean(DV_COL, na.rm=TRUE),     DV_COL = as.name(describe_column) ),
                       SD   = interp( ~sd(DV_COL, na.rm=TRUE),       DV_COL = as.name(describe_column) ),
                       Var  = interp( ~var(DV_COL, na.rm=TRUE),      DV_COL = as.name(describe_column) ),
                       Min  = interp( ~mean(DV_COL, na.rm=TRUE)*1.0, DV_COL = as.name(describe_column) ),
                       Max  = interp( ~mean(DV_COL, na.rm=TRUE)*1.0, DV_COL = as.name(describe_column) )
  )
  if (data_options$trial_column %in% colnames(data)) summarize_expr$NumTrials = interp( ~n_distinct(TRIAL_COL), TRIAL_COL = as.name(data_options$trial_column))

  # Group, Summarize
  df_grouped <- group_by_(data, .dots = as.list(group_columns))
  df_summarized <- summarize_(df_grouped, .dots =summarize_expr )
  return(df_summarized)

}
