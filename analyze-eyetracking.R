# analyze-eyetracking.R
#
# @author Brock Ferguson
#         brock.ferguson@gmail.com
#         brockferguson.com
#
# @author Jacob Dink
#         jacobwdink@gmail.com
#         github.com/jwdink

# Loading/Cleaning/Describing ------------------------------------------------------------------------
#' 
#' set_data_options
#' 
#' Create a list of data options which is passed to most of these
#' methods.
#' 
#' @param character          participant_column Column name for participant identifier
#' @param character          trackloss_column   Column name indicating trackloss
#' @param character          time_column        Column name indicating time
#' @param character          trial_column       Column name indicating trial identifier
#' @param character.vector   item_columns       Column names indicating items (can be same as trial_column)
#' @param character.vector   aoi_columns        Names of AOIs
#' 
#' 
#' @return list of configuration options

set_data_options <- function(
  participant_column,
  trackloss_column,
  time_column,
  trial_column,
  item_columns = NULL,
  aoi_columns
) {
  list(
    participant_column = participant_column,
    trackloss_column = trackloss_column,
    time_column = time_column,
    trial_column = trial_column,
    item_columns = item_columns,
    aoi_columns = aoi_columns
  )
}

#' verify_dataset()
#' 
#' Verify, and fix, the status of the dataset by assessing columns in your data_options. 
#' 
#' @param dataframe data
#' @param list data_options
#' 
#' @return dataframe data

verify_dataset <- function(data, data_options) {
  out = data
  
  if (!all(data_options$aoi_columns %in% colnames(data))) {
    stop('Not all of the AOI columns specified in data_options are in the data.')
  }
  
  as.numeric2 = function(x) as.numeric(as.character(x))
  check_then_convert = function(x, checkfunc, convertfunc, colname) { 
    if ( !checkfunc(x) ) {
      message('Converting ' , colname ,' to proper type.')
      convertfunc(x)
    } else {
      x
    }
  }
  col_type_converter = list("participant_column" = function(x) check_then_convert(x, is.factor, as.factor, "Participants"),
                            "time_column"        = function(x) check_then_convert(x, is.numeric, as.numeric2, "Time"),
                            "trial_column"       = function(x) check_then_convert(x, is.factor, as.factor, "Trial"),
                            "trackloss_column"   = function(x) check_then_convert(x, is.logical, as.logical, "Trackloss"),
                            "item_columns"       = function(x) check_then_convert(x, is.factor, as.factor, "Item"),
                            "aoi_columns"        = function(x) check_then_convert(x, is.logical, as.logical, "AOI")
  )
  
  for (col in names(col_type_converter) ) {
    for(i in seq_along(data_options[[col]])) {
      if (is.null(out[[ data_options[[col]][i] ]])) stop("Data are missing: ", col)
      out[[ data_options[[col]][i] ]] = col_type_converter[[col]]( out[[ data_options[[col]][i] ]] )
    }
  }
  
  return( out )
}

#' subset_by_window ()
#' 
#' Extract a subset of the dataset, where each trial falls inside a time-window.
#' Time-window can either be specifed by a number (for a timestamp across all trials) or
#' by a column which picks out a timestamp for each participant/trial
#' 
#' @param dataframe data
#' @param list data_options
#' @param numeric/character window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param numeric/character window_end Number (for timestamp) or character (for column that specifies timestamp)
#' @param logical rezero Should the beginning of the window be considered the zero point of the timestamp? 
#'                       Default TRUE when window_start is column, FALSE when window_start is number
#' 
#' @return dataframe 

subset_by_window = function(data, data_options, window_start = -Inf, window_end = Inf, rezero = NULL) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  # Prelims:
  time_col = as.name(data_options$time_column)
  
  # Window Start:
  if (is.character(window_start)) {
    data$.WindowStart = data[[window_start]]
    if (is.null(rezero)) rezero = TRUE
  } else {
    data$.WindowStart = window_start
    if (is.null(rezero)) rezero = FALSE
  }
  
  # Window End:
  if (is.character(window_end)) {
    data$.WindowEnd = data[[window_end]]
  } else {
    data$.WindowEnd = window_end
  }
  
  # Subset
  df_subsetted = filter_(.data = data, 
                         .dots = list( interp(~ TIME_COL >= .WindowStart & TIME_COL <= .WindowEnd, 
                                              TIME_COL= time_col)
                         ))

  # Rezero
  if (rezero) {
    df_grouped = group_by_(.data = df_subsetted, .dots = list(data_options$participant_column, data_options$trial_column) )
    df_rezeroed = mutate_(.data= df_grouped, 
                          .dots = list(.NewTimeStamp =interp( ~ TIME_COL - .WindowStart, TIME_COL = time_col)
                          )) 
    out = ungroup(df_rezeroed)
    out[[data_options$time_column]] = out[[".NewTimeStamp"]]
    out[[".NewTimeStamp"]] = NULL
  } else {
    out = df_subsetted
  }
  
  out[[".WindowStart"]] = NULL
  out[[".WindowEnd"]] = NULL
  
  out
}


#' describe_data ()
#' 
#' Describe a DV column in the dataset by a (group of) factor(s)
#' 
#' @param dataframe data
#' @param list data_options
#' @param character dv
#' @param character.vector factors
#' 
#' @return dataframe 

describe_data = function(data, data_options, dv, factors) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  data %>%
    group_by_(.dots = as.list(factors)) %>%
    summarise_(.dots = list(Mean = interp( ~mean(DV_COL, na.rm=TRUE),     DV_COL = as.name(dv) ),
                            SD   = interp( ~sd(DV_COL, na.rm=TRUE),       DV_COL = as.name(dv) ),
                            Var  = interp( ~var(DV_COL, na.rm=TRUE),      DV_COL = as.name(dv) ),
                            Min  = interp( ~mean(DV_COL, na.rm=TRUE)*1.0, DV_COL = as.name(dv) ),
                            Max  = interp( ~mean(DV_COL, na.rm=TRUE)*1.0, DV_COL = as.name(dv) ),
                            NumTrials = interp( ~n_distinct(TRIAL_COL), TRIAL_COL = as.name(data_options$trial_column))
    ))
  
}


#' trackloss_analysis ()
#' 
#' Get information on trackloss
#' 
#' @param dataframe data
#' @param list data_options
#' @param numeric/character window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param numeric/character window_end Number (for timestamp) or character (for column that specifies timestamp)
#' 
#' @return dataframe 

trackloss_analysis = function(data, data_options, window_start = -Inf, window_end = Inf) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  trackloss_col = as.name(data_options$trackloss_column)
  
  # Filter by Time-Window:
  df_subsetted = subset_by_window(data, data_options, window_start, window_end)
  
  # Get Trackloss-by-Trial:
  df_grouped_trial = group_by_(df_subsetted, .dots = list(data_options$participant_column, data_options$trial_column))
  df_trackloss_by_trial = mutate_(df_grouped_trial,
                                  .dots = list(SumTracklossForTrial = interp(~sum(TRACKLOSS_COL, na.rm=TRUE), TRACKLOSS_COL = trackloss_col),
                                               TotalTrialLength     = interp(~length(TRACKLOSS_COL), TRACKLOSS_COL = trackloss_col),
                                               TracklossForTrial    = interp(~SumTracklossForTrial / TotalTrialLength)
                                  )) 

  # Get Trackloss-by-Participant:
  df_grouped_ppt = group_by_(df_trackloss_by_trial, .dots = list(data_options$participant_column))
  df_trackloss_by_ppt = mutate_(df_grouped_ppt,
                                .dots = list(SumTracklossForParticipant = interp(~sum(TRACKLOSS_COL, na.rm=TRUE), TRACKLOSS_COL = trackloss_col),
                                             TotalParticipantLength     = interp(~length(TRACKLOSS_COL), TRACKLOSS_COL = trackloss_col),
                                             TracklossForParticipant    = interp(~SumTracklossForParticipant / TotalParticipantLength)
                                ))
  
  # Get Z-Scores:
  df_grouped = group_by_(df_trackloss_by_ppt, .dots = list(data_options$participant_column, data_options$trial_column) )
  df_summarized = summarise(df_grouped,
                            Samples = mean(TotalTrialLength, na.rm=TRUE),
                            TracklossSamples = mean(SumTracklossForTrial, na.rm=TRUE),
                            TracklossForTrial = mean(TracklossForTrial, na.rm=TRUE),
                            TracklossForParticipant = mean(TracklossForParticipant, na.rm=TRUE))
  df_summarized = ungroup(df_summarized)
  
  return(df_summarized)
}

#' clean_by_trackloss ()
#' 
#' Remove trials/participants with too much trackloss, with a customizable threshold
#' 
#' @param dataframe data
#' @param list data_options
#' @param numeric participant_prop_thresh Maximum proportion of trackloss for participants
#' @param numeric trial_prop_thresh Maximum proportion of trackloss for trials
#' @param numeric/character window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param numeric/character window_end Number (for timestamp) or character (for column that specifies timestamp)
#' 
#' @return dataframe 

clean_by_trackloss = function(data, data_options, 
                              participant_prop_thresh = 1, trial_prop_thresh = 1,
                              window_start = -Inf, window_end = Inf) {
  data$.TrialID = paste(data[[data_options$participant_col]], data[[data_options$trial_col]], sep = "_")
  
  # Trackloss Analysis:
  message("Performing Trackloss Analysis...")
  tl = trackloss_analysis(data, data_options, window_start, window_end)
  
  # Bad Trials:
  if (trial_prop_thresh < 1) {
    message("Will exclude trials whose trackloss proportion is greater than : ", trial_prop_thresh)
    exclude_trials_props = paste(tl$Participant[tl$TracklossForTrial > trial_prop_thresh], 
                                 tl$Trial[tl$TracklossForTrial > trial_prop_thresh], 
                                 sep="_")
    message(paste("\t...removed ", length(exclude_trials_props), " trials."))
  } else {
    exclude_trials_props = c()
  }
  
  # Bad Participants
  part_vec = data[[data_options$participant_col]]
  if (participant_prop_thresh < 1) {
    message("Will exclude participants whose trackloss proportion is greater than : ", participant_prop_thresh)
    exclude_ppts_prop = unique(tl$Participant[tl$TracklossForParticipant > participant_prop_thresh])
    message(paste("\t...removed ", length(exclude_ppts_prop), " participants."))
  } else {
    exclude_ppts_prop = c()
  }
  
  exclude_trials = c(exclude_trials_props,
                     unique( data$.TrialID[part_vec %in% exclude_ppts_prop] ))
  
  # Remove:
  data_clean = filter(data, ! .TrialID %in% exclude_trials)
  data_clean$.TrialID = NULL
  
  return(data_clean)
  
}

#' convert_non_aoi_to_trackloss ()
#' 
#' Should gaze outside of any AOI be considered trackloss? This function sets trackloss to TRUE for any 
#' samples that were not inside an AOI
#' 
#' @param dataframe data
#' @param list data_options
#' 
#' @return dataframe 

convert_non_aoi_to_trackloss = function(data, data_options) {
  
  # Create version of AOIs with no NAs:
  .narepl = function(x) ifelse(is.na(x), 0, x)
  data_no_na = mutate_each_(data, funs(.narepl), vars = sapply(data_options$aoi_columns, as.name))
  
  # Find all rows which have no AOIs in them:
  data_no_na[[".AOISum"]] = 0
  for (aoi in data_options$aoi_columns) {
    data_no_na[[".AOISum"]] = data_no_na[[".AOISum"]] + data_no_na[[aoi]]
  }
  
  # Set these rows as trackloss:
  data[[data_options$trackloss_column]] = ifelse(data_no_na[[".AOISum"]] == 0, TRUE, data_no_na[[data_options$trackloss_column]])
  
  return(data)
}

#' keep_trackloss ()
#' 
#' Converts data so that, in all subsequent proportion-looking calculations, 
#' proportion_looking_at_aoi = time_looking_at_aoi / time_possible.
#' Trackloss is not considered missing data, but instead is counted as time
#' *not* spent looking at each AOI.
#' 
#' @param dataframe data
#' @param list data_options
#' 
#' @return dataframe 

keep_trackloss = function(data, data_options) {
  
  data = ungroup(data)
  
  # Create version of AOIs which = FALSE for any trackloss samples:
  for (aoi in data_options$aoi_columns) {
    data[[aoi]] = ifelse(data[[data_options$trackloss_column]]==1, FALSE, data[[aoi]] )
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
    data[[aoi]] = ifelse(is.na(data[[aoi]]), FALSE, data[[aoi]] )
  }
  return(data)
}

#' remove_trackloss ()
#' 
#' Converts data so that, in all subsequent proportion-looking calculations, 
#' proportion_looking_at_aoi = time_looking_at_aoi / time_looking
#' Trackloss *is* considered missing data, and time spent not looking at a given AOI
#' must have been spent looking at another AOI
#' 
#' @param dataframe data
#' @param list data_options
#' @param logical delete_rows (default: TRUE) Should rows with trackloss be deleted?
#' 
#' @return dataframe 
remove_trackloss = function(data, data_options, delete_rows = TRUE) {
  require('dplyr', quietly=TRUE)
  
  data = ungroup(data)
  
  if (delete_rows == TRUE) {
    
    # We want to only remove rows that are positively identified as trackloss, so we replace any trackloss=NA with trackloss=FALSE
    data[[".TracklossBoolean"]] = ifelse(is.na(data[[data_options$trackloss_column]]), FALSE, data[[data_options$trackloss_column]])

    # Remove all rows with Trackloss:
    data = filter(data, .TracklossBoolean == 0)
    out = data[, !(colnames(data) %in% c('.TracklossBoolean'))]
    
  } else {
    # Set Looking-at-AOI to NA for any samples where there is Trackloss:
    
    # TODO: fix bug that removes all AOI looks when delete_rows == FALSE
    
    out = data
    for (aoi in data_options$aoi_columns) {
      out[[aoi]] = ifelse(data[[data_options$trackloss_column]]==1, NA, data[[aoi]] )
    }
  }
  
  out
}

# Shaping ------------------------------------------------------------------------------------------

#' window_shape()
#' 
#' Collapse time across our entire window and return a dataframe ready for analyses (e.g., lmer)
#' 
#' @param dataframe data
#' @param list data_options
#' @param character aoi                         Which AOIs are of interest? Defaults to all in 'data_options'
#' @param character.vector condition_columns    Which columns indicate conditions, and therefore should be 
#'                                              preserved in grouping operations?
#' @param character summarize_by                Should the data be summarized along, e.g., participants, items, etc.
#'                                              If so, give column names here. If left blank, will leave trials distinct.
#'                                              The former is needed for more traditional analyses (t.tests, ANOVAs), while
#'                                              the latter is preferable for mixed-effects models (lmer)
#' 
#' @return dataframe
window_shape <- function(data, 
                         data_options, 
                         aoi = data_options$aoi_columns, 
                         condition_columns = NULL,
                         summarize_by = NULL
) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  # For Multiple AOIs:
  if (length(aoi) > 1) {
    list_of_dfs = lapply(X = aoi, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      window_shape(data, data_options, aoi = this_aoi, condition_columns, summarize_by)
    })
    out = bind_rows(list_of_dfs)
    attrs = attr(out,"eyetrackingR")
    new_attrs = list(summarized_by = summarize_by)
    attr(out,"eyetrackingR") = as.list(c(attrs, new_attrs))
    out = as.data.frame(out)
    class(out) = c('window_shape', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data)
  aoi_col = as.name(aoi)

  # Make Summary
  if (is.null(summarize_by)) {
    groups = c(data_options$participant_column, 
               data_options$item_columns,
               data_options$trial_column, 
               condition_columns)
  } else {
    groups = c(summarize_by, condition_columns)
  }
  out = .make_proportion_looking_summary(data=data, groups = groups, aoi_col)
  
  out = as.data.frame(out)
  class(out) = c('window_shape', class(out))
  attrs = attr(out,"eyetrackingR")
  new_attrs = list(summarized_by = summarize_by)
  attr(out,"eyetrackingR") = as.list(c(attrs, new_attrs))
  return(out)
  
}

#' time_shape()
#' 
#' Creates time-bins and summarizes proportion-looking within each time-bin
#' 
#' @param dataframe data
#' @param list data_options
#' @param numeric time_bin_size
#' @param character aoi                         Which AOIs are of interest? Defaults to all in 'data_options'
#' @param character.vector condition_columns    Which columns indicate conditions, and therefore should be 
#'                                              preserved in grouping operations?
#' @param character summarize_by                Should the data be summarized along, e.g., participants, items, etc.?
#'                                              If so, give column name(s) here. If left blank, will leave trials distinct.
#'                                              The former is needed for more traditional analyses (t.tests, ANOVAs), while
#'                                              the latter is preferable for mixed-effects models (lmer)
#' 
#' @return dataframe summarized

time_shape <- function (data, 
                        data_options, 
                        time_bin_size, 
                        aoi = data_options$aoi_columns, 
                        condition_columns = NULL,
                        summarize_by = NULL) {
  
  
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  # For Multiple AOIs:
  if (length(aoi) > 1) {
    list_of_dfs = lapply(X = aoi, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      time_shape(data, data_options, time_bin_size, this_aoi, condition_columns, summarize_by)
    })
    out = bind_rows(list_of_dfs)
    attrs = attr(out,"eyetrackingR")
    new_attrs = list(summarized_by = summarize_by)
    attr(out,"eyetrackingR") = as.list(c(attrs, new_attrs))
    out = as.data.frame(out)
    class(out) = c('time_shape', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data)
  aoi_col = as.name(aoi)
  
  # Make Time Bin:
  data[["TimeBin"]] = floor(data[[data_options$time_column]] / time_bin_size)
  
  # Make Summary
  if (is.null(summarize_by)) {
    groups = c(data_options$participant_column, 
               data_options$item_columns,
               data_options$trial_column, 
               condition_columns, "TimeBin")
  } else {
    groups = c(summarize_by, condition_columns, "TimeBin")
  }
  df_summarized = .make_proportion_looking_summary(data=data, groups = groups, aoi_col)
  df_summarized[["Time"]] = df_summarized[["TimeBin"]] * time_bin_size
  
  # Add orthogonal polynomials for Growth Curve Analyses
  time_bin_column <- df_summarized$TimeBin
  max_degree = min( length(unique(time_bin_column))-1 , 7 )
  if (max_degree < 7) warning("Fewer time bins than polynomial degrees-- consider decreasing size of time bin.")
  orthogonal_polynomials <- poly(sort(as.vector(unique(time_bin_column))), max_degree)
  time_codes <- data.frame(
    sort(as.vector(unique(time_bin_column))),
    orthogonal_polynomials[, c(1:max_degree)]
  )
  colnames(time_codes) <- c('TimeBin',paste0("ot", 1:max_degree))
  
  out <- left_join(df_summarized, time_codes, by='TimeBin')
  
  out = as.data.frame(out)
  class(out) = c('time_shape', class(out))
  attrs = attr(out,"eyetrackingR")
  new_attrs = list(summarized_by = summarize_by)
  attr(out,"eyetrackingR") = as.list(c(attrs, new_attrs))
  return(out)
  
}


#' onset_shape()
#' 
#' divide trials into which AOI they started on; augment with column indicating switch away from that AOI
#' 
#' @param dataframe data            The original (verified) data
#' @param list data_options
#' @param numeric onset_time        When should we check for their "starting" AOI? 
#' @param numeric fixation_window_length       Smoothes the data by determining the fixated AOI using a moving average over time bins that are this long
#'                                             (e.g., "100" means that the fixated AOI is determined over a 100ms rolling window)
#' @param character target_aoi      Which AOI is the target that should be switched *to*
#' @param character distractor_aoi  Which AOI is the distractor that should be switched *from* (default = !target_aoi)
#' 
#' @return dataframe 

onset_shape = function(data, data_options, onset_time, fixation_window_length, target_aoi, distractor_aoi = NULL) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  require("zoo", quietly=TRUE)
  
  ## Helper Function:
  na_replace_rollmean = function(col) {
    col = ifelse(is.na(col), 0, col)
    rollmean(col, k = fixation_window_length_rows, partial=TRUE, fill= NA, align="left")
  }
  
  ## Prelims:
  if (is.null(distractor_aoi)) {
    distractor_aoi = paste0("NOT_", target_aoi)
    data[[distractor_aoi]] = !data[[target_aoi]]
  }
  time_col = as.name(data_options$time_column)
  
  ## Translate TimeWindow units from time to number of rows (for rolling mean):
  df_time_per_row = group_by_(data, .dots = c(data_options$participant_column, data_options$trial_column) )
  df_time_per_row = summarise_(df_time_per_row,
                               .dots = list(TimePerRow = interp(~mean(diff(TIME_COL)), TIME_COL = time_col)
                                            ))
  time_per_row = round(mean(df_time_per_row[["TimePerRow"]]))
  fixation_window_length_rows = fixation_window_length / time_per_row
  
  ## Determine First AOI, Assign Switch Value for each timepoint
  
  # Group by Ppt*Trial:
  df_grouped = group_by_(data, .dots = list(data_options$participant_column, data_options$trial_column) )
  
  # Create a rolling-average of 'inside-aoi' logical for target and distractor, to give a smoother estimate of fixations
  df_smoothed = mutate_(df_grouped,
                        .dots = list(.Target    = interp(~na_replace_rollmean(TARGET_AOI), TARGET_AOI = as.name(target_aoi)),
                                     .Distractor= interp(~na_replace_rollmean(DISTRACTOR_AOI), DISTRACTOR_AOI = as.name(distractor_aoi)),
                                     .Time      = interp(~TIME_COL, TIME_COL = time_col)
                                     ))
  
  # For any trials where no data for onset timepoint is available, find the closest timepoint. 
  # Calculate FirstAOI
  df_first_aoi = mutate(df_smoothed,
                        .ClosestTime = ifelse(length(which.min(abs(.Time - onset_time)))==1, .Time[which.min(abs(.Time - onset_time))], NA),
                        FirstAOI     = ifelse(.Target[.Time==.ClosestTime] > .Distractor[.Time==.ClosestTime], target_aoi, distractor_aoi)
  )
  df_first_aoi = ungroup(df_first_aoi)
  
  # If closest timepoint was too far away from onset window, record FirstAOI as unknown
  # Create a column specifying whether they have switched away from FirstAOI
  out = mutate(df_first_aoi,
         FirstAOI  = ifelse(abs(.ClosestTime-onset_time) > fixation_window_length, NA, FirstAOI),
         WhichAOI  = ifelse(.Target > .Distractor, target_aoi, distractor_aoi),
         SwitchAOI = FirstAOI != WhichAOI)
  out = select(out, -.Target, -.Distractor, -.Time, -.ClosestTime)

  # Assign class information:
  out = as.data.frame(out)
  class(out) = c('onset_shape', class(out))
  attr(out, 'eyetrackingR') = list(onset_contingent = list(distractor_aoi = distractor_aoi, 
                                                           target_aoi = target_aoi, 
                                                           onset_time = onset_time,
                                                           fixation_window_length = fixation_window_length))
  
  return(out)
}

#' switch_shape()
#' 
#' takes trials split by initial-AOI, and determines how quickly subjects switch away from that AOI
#' 
#' @param dataframe data The output of "onset_shape"
#' @param list data_options
#' @param character.vector condition_columns
#' 
#' @return dataframe 
#' 
switch_shape = function(data, ...) {
  UseMethod("switch_shape")
}
switch_shape.onset_shape = function(data, data_options, condition_columns=NULL) {

  time_col = as.name(data_options$time_column)
  
  df_cleaned = filter(data, !is.na(FirstAOI))
  df_grouped = group_by_(data, 
                         .dots = c(data_options$participant_column, 
                                   data_options$trial_column, 
                                   data_options$item_columns, 
                                   "FirstAOI", 
                                   condition_columns))
  df_summarized = summarise_(df_grouped,
                            .dots = list(FirstSwitch = interp(~TIME_COL[first(which(SwitchAOI), order_by= TIME_COL)], TIME_COL = time_col)
                                         ))
  
  df_summarized = as.data.frame(df_summarized)
  class(df_summarized) = c('switch_shape', class(df_summarized))
  
  return(df_summarized)
}


# bootstrapped_shape(data, data_options, condition_column, within_subj, samples, resolution, alpha)
#
# Bootstrap splines from a time_shape() shape. Return bootstrapped splines.
#
# @param dataframe data a Time shape dataset
# @param list data_options Standard list of options for manipulating dataset
# @param character factor What factor to split by? Maximum two conditions!
# @param boolean within_subj Are the two conditions within or between subjects?
# @param int samples How many (re)samples to take?
# @param numeric resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100 intervals per second, or hundredths of a second
# @param numeric alpha p-value when the groups are sufficiently "diverged"
# @param character smoother Smooth data using "smooth.spline," "loess," or leave NULL for no smoothing 
# 
# @return dataframe
bootstrapped_shape = function(data, ...) {
  UseMethod("bootstrapped_shape")
}

bootstrapped_shape.time_shape <- function (data, data_options, condition_column, aoi = NULL, within_subj = FALSE, samples = 1000, resolution = 10, alpha = .05, smoother = 'none') {
  require("dplyr", quietly=TRUE)
  if (!require("pbapply")) {
    pbreplicate = function(n, expr, simplify) replicate(n, expr, simplify)
    message("Install package 'pbapply' for a progress bar in this function.")
  }

  # validate arguments
  if ( length(levels(as.factor(data[[condition_column]]))) != 2 ) {
    stop('bootstrapped_shape requires a condition_column with exactly 2 levels.')
  }
  
  if (!(smoother %in% c('smooth.spline','loess','none'))) {
    stop('bootstrapped_shape requires that "smoother" be set to "none", "smooth.spline", or "loess")')
  }
  
  # Pre-prep data
  if (is.null(aoi)) {
    if (length(unique(data$AOI)) > 1) stop("Please specify which AOI you wish to bootstrap.")
  } else {
    data = filter(data, AOI == aoi)
  }
  data = data[ !is.na(data[[condition_column]]), ]

  # define sampler/bootstrapper:
  sampler <- function (run_original, run_subjects_rows, data_options, resolution, smoother) {

    # Take a list where each element corresponds to the rows of the subject. 
    # Sample w/o replacement N elements, and concatenate their contents
    # This gives you a vector specifying which rows to extract, in order to extract the data corresponding to the sampled subjects
    sampled_subject_rows = unlist(sample(run_subjects_rows, length(run_subjects_rows), replace = TRUE))
    run_data = run_original[sampled_subject_rows,]
    
    # get timepoints
    run_times <- unique(run_original$Time)
    run_times <- run_times[order(run_times)]
    
    if (smoother == "none") {
      # use straight linear approximation on the values
      run_predicted_times <- seq(min(run_times), max(run_times), by=resolution)
      
      run_predictions <- with(run_data,
                              approx(run_data$Time, run_data$Prop, xout=run_predicted_times))
      return(run_predictions$y)
    }
    else if (smoother == 'smooth.spline') {
      # spline! 
      # with generalized cross-validation setting smoothing parameter
      run_spline <- with(run_data,
                         smooth.spline(Time, Prop, cv=FALSE))
      
      # get interpolated spline predictions for total time at *resolution*
      run_predicted_times <- seq(min(run_times), max(run_times), by=resolution)
      run_predictions <- predict(run_spline, run_predicted_times)
      
      return(run_predictions$y)
    }
    else if (smoother == 'loess') {
      # loess!
      run_loess <- with(run_data,
                        loess(Prop ~ Time))
      
      # get interpolated loess predictions for total time at *resolution*
      run_predicted_times <- seq(min(run_times), max(run_times), by=resolution)
      run_predictions <- predict(run_loess, run_predicted_times)
      
      return (run_predictions)
    }
  }
  
  # this dataframe will hold our final dataset
  combined_bootstrapped_data <- data.frame()
  
  # re-factor Participant name column, so that levels() is accurate
  data[[data_options$participant_column]] <- factor(data[[data_options$participant_column]])
  
  if (within_subj == FALSE) {
    # between-subjects:
    for (level in unique(data[[condition_column]]) ) {
      # subset for condition level
      subsetted_data <- data[which(data[, condition_column] == level),]
      subsetted_data$RowNum = 1:nrow(subsetted_data)
      
      # get subjects
      run_subjects <- unique(subsetted_data[[data_options$participant_column]])
      run_subjects_rows = lapply(run_subjects, function(sub) subsetted_data$RowNum[ subsetted_data[[data_options$participant_column]] == sub ])

      # bootstrap
      message('Resampling ', level, "...")
      bootstrapped_data <- pbreplicate(samples, sampler(subsetted_data, run_subjects_rows, data_options, resolution, smoother))
      bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))
      
      # label each sample by number
      sample_rows <- paste('Sample', c(1:samples), sep="")
      colnames(bootstrapped_data) <- sample_rows
      
      bootstrapped_data <- data.frame(stringsAsFactors = FALSE,
        condition_column = level,
        Time = seq(min(subsetted_data$Time), max(subsetted_data$Time), by=resolution),
        bootstrapped_data
      )
      colnames(bootstrapped_data)[1] <- condition_column
      
      # 
      combined_bootstrapped_data <- bind_rows(combined_bootstrapped_data,bootstrapped_data)
    }
  }
  else {
    # within-subjects:
    
    # Group by participant, timebin; Calculate the difference in proportion between level1 and level2
    level1 = levels(data[[condition_column]])[1]
    level2 = levels(data[[condition_column]])[2]
    df_grouped = group_by_(data, .dots = c(data_options$participant_column, "Time") ) 
    df_diff = summarise_(df_grouped, 
                         .dots = list(Prop1 = interp(~mean(Prop[CONDITION_COL == level1]), CONDITION_COL = as.name(condition_column)),
                                      Prop2 = interp(~mean(Prop[CONDITION_COL == level2]), CONDITION_COL = as.name(condition_column)),
                                      Prop  = interp(~Prop1 - Prop2)
                         ))
    
    # remove all samples where Prop == NA;
    df_diff <- df_diff[!is.na(df_diff$Prop), ]

    df_diff$RowNum = 1:nrow(df_diff)
    
    # get subjects
    message("Preparing dataframe...")
    run_subjects <- unique(df_diff[[data_options$participant_column]])
    run_subjects_rows = lapply(run_subjects, function(sub) df_diff$RowNum[ df_diff[[data_options$participant_column]] == sub ])

    # bootstrap
    message('Resampling ...')
    bootstrapped_data <- pbreplicate(samples, sampler(df_diff, run_subjects_rows, data_options, resolution, smoother))
    bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))

    sample_rows <- paste('Sample', c(1:samples), sep="")
    colnames(bootstrapped_data) <- sample_rows
    
    bootstrapped_data <- data.frame(
      Time = seq(min(data$Time), max(data$Time), by=resolution),
      bootstrapped_data
    )

    combined_bootstrapped_data <- bootstrapped_data
  }
  
  # Assign class information:
  combined_bootstrapped_data = as.data.frame(combined_bootstrapped_data)
  class(combined_bootstrapped_data) = c('bootstrapped_shape', class(combined_bootstrapped_data))
  attr(combined_bootstrapped_data, 'eyetrackingR') = list(
    bootstrapped = list(within_subj = within_subj,
                        condition_column = condition_column,
                        samples = samples,
                        alpha = alpha,
                        resolution = resolution,
                        min_time = min(combined_bootstrapped_data[['Time']])
    ))
  
  return(combined_bootstrapped_data)
}

# Analyzing ------------------------------------------------------------------------------------------

#' analyze_time_clusters.time_shape()
#' 
#' Takes data that has been summarized into time-bins, and finds adjacent time bins that
#' pass some threshold of significance, and assigns these adjacent groups into clusters
#' for further examination.
#' 
#' @param dataframe.time_shape data The output of the 'time_shape' function
#' @param list data_options            
#' ...
#' @return dataframe 
analyze_time_clusters = function(data, data_options, condition_column, method, alpha, ...) {
  UseMethod("analyze_time_clusters")
}
analyze_time_clusters.time_shape = function(data, 
                                       data_options,
                                       condition_column,
                                       aoi = NULL,
                                       test = "t.test",
                                       within_subj = FALSE,
                                       threshold = NULL,
                                       alpha = .05,
                                       formula = NULL,
                                       samples = 1000,
                                       ...) {
  
  
  ## Helper:
  .label_clusters = function(vec) {
    vec = c(0,vec)
    vec[is.na(vec)] = 0
    out = c(cumsum(diff(vec)==1))
    out[!vec] = NA
    out[-1]
  }
  
  # Filter Data:
  if (is.null(aoi)) {
    if (length(unique(data$AOI)) == 1) {
      aoi = unique(data$AOI)
      data = filter(data, AOI == aoi)
    } else {
      stop("Please specify the AOI column.")
    }
  } else {
    data = filter(data, AOI == aoi)
  }

  # Arg check:
  if (test %in% c("t.test", "wilcox.test")) {
    paired = list(...)[['paired']]
    if (within_subj==TRUE) {
      # if within_subj is true, we need to confirm they overrode default
      if (!identical(paired, TRUE)) stop("For ", test, ", if 'within_subj' is TRUE, then 'paired' must also be TRUE.")
    } else {
      # if within_subj is false, we just need to confirm they didn't set paired = TRUE
      if (identical(paired, TRUE)) stop("For ", test, ", if 'within_subj' is FALSE, then 'paired' must also be FALSE.")
    }
  } else if (test == "lm") {
    if (within_subj == TRUE) stop("For lm, 'within_subj' must be FALSE.")
  } 
  
  # Compute Time Bins:
  time_bin_summary = analyze_time_bins(data, data_options, 
                                       condition_column = condition_column,
                                       test = test,
                                       threshold = threshold,
                                       alpha = alpha,
                                       formula = formula, 
                                       return_model = FALSE,
                                       ...)
  
  # Label Adjacent Clusters:
  # TO DO: do so separately for neg and for pos
  time_bin_summary$Significant = 
    time_bin_summary$Statistic >= time_bin_summary$CritStatisticPos | time_bin_summary$Statistic <= time_bin_summary$CritStatisticNeg
  time_bin_summary$Cluster = .label_clusters(time_bin_summary$Significant)
  
  # Compute Sum Statistic for each Cluster
  sum_stat = c()
  for (clust in na.omit(unique(time_bin_summary$Cluster))) {
    sum_stat[clust] = sum(time_bin_summary$Statistic[which(time_bin_summary$Cluster==clust)])
  }
  
  # Find the cluster with the biggest sum statistic, get the original data for that cluster
  biggest_cluster = which.max(sum_stat)
  df_timeclust = left_join(data, time_bin_summary[,c('TimeBin','AOI','Cluster')], by=c('TimeBin','AOI'))
  df_biggclust = filter(df_timeclust, Cluster == biggest_cluster)# df_timeclust[which(df_timeclust$Cluster==biggest_cluster),]
  
  # Resample this data and get sum statistic each time, creating null distribution
  if (within_subj) {
    
    # unique conditions, rows:
    participants = unique(df_biggclust[[data_options$participant_column]])
    conditions = unique(df_biggclust[[condition_column]])
    
    # get a list of list of rows. outer list corresponds to participants, inner to conditions
    foo = lapply(X = participants, FUN = function(ppt) {
      lapply(X = conditions, FUN = function(cond) {
        which(df_biggclust[[data_options$participant_column]] == ppt & df_biggclust[[condition_column]] == cond)
      })
    })
    
    null_distribution = rep(NA, samples)
    for (iter in 1:samples) {
      cat(".")
      df_resampled = df_biggclust
      
      # for each participant, randomly select (w/replacement) rows to be assigned to each condition
      for (list_of_rows in foo) {
        resampled = sample(x = list_of_rows, size = length(list_of_rows), replace = TRUE)
        for (i in seq_along(resampled)) {
          rows = resampled[[i]]
          df_resampled[rows,condition_column] = conditions[i]
        }
      }
      
      # this gives a dataframe where the "condition" label has been resampled within each participant 
      # run analyze time bins on it to get sum statistic for cluster
      time_bin_summary_resampled = analyze_time_bins(df_resampled, data_options, 
                                                     condition_column = condition_column,
                                                     test = test,
                                                     threshold = threshold,
                                                     alpha = alpha,
                                                     formula = formula, 
                                                     return_model = FALSE,
                                                     quiet = TRUE,
                                                     ...)
      
      null_distribution[iter] = sum(time_bin_summary_resampled$Statistic, na.rm=TRUE)
      
    }
    
    
    
  } else {
    # between subjects
    # to do (easier?)
  }
  cat("\n")
  
  out = list(null_distribution = null_distribution,
             data = df_timeclust,
             cluster_sum_stats = sum_stat)
  
  return(out)
  
}

#' analyze_time_bins()
#' 
#' Runs a test on each time-bin of a time-analysis. Defaults to a t-test, but supports wilcox, lm, and lmer as well.
#' 
#' @param dataframe.time_shape data   The output of the 'time_shape' function
#' @param list data_options
#' @param character condition_column  The variable whose test statistic you are interested in
#' @param numeric threshold           Value of statistic used in determining significance
#' @param numeric alpha               Alpha value for determining significance, ignored if threshold is given
#' @param character test              What type of test should be performed in each time bin? Supports t.test, wilcox, lm, or lmer.
#' @param character formula           What formula should be used for the test? Optional (for all but lmer), if unset will use Prop ~ condition_column
#' @param logical return_model        In the returned dataframe, should a model be given for each time bin, or just the summary of that model?
#' @param ... ...                     Any other arguments to be passed to the selected 'test' function (e.g., paired, var.equal, etc.)
#' ...
#' @return dataframe 
analyze_time_bins = function(data, ...) {
  UseMethod("analyze_time_bins")
}
analyze_time_bins.time_shape <- function(data, 
                              data_options, 
                              condition_column,
                              test = "t.test",
                              threshold = NULL,
                              alpha = .05,
                              formula = NULL,
                              return_model = FALSE,
                              quiet = FALSE,
                              ...)
{
  
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  require('broom', quietly=TRUE)
  require("pbapply", quietly = TRUE)
  
  if (!test %in% c("t.test","wilcox.test","lm","lmer")) stop('Test must be "t.test","wilcox.test","lm", or "lmer".')

  # For Multiple aois:
  if (!'AOI' %in% colnames(data)) stop("'AOI' column is missing from data.")
  aois = unique(data[['AOI']])
  if ( length(aois) > 1 ) {
    list_of_dfs = lapply(X = aois, FUN = function(this_aoi) {
      if (!quiet) message("Analyzing ", this_aoi, "...")
      this_df = filter(data, AOI == this_aoi)
      class(this_df) = class(data)
      analyze_time_bins(data = this_df, data_options, condition_column, test, threshold, alpha, formula, return_model, quiet, ...)
    })
    out = bind_rows(list_of_dfs)
    out = as.data.frame(out)
    class(out) = c('bin_analysis', class(out))
    return( out )
  }
  
  # Check that data is collapsed by participants:
  if (test != "lmer") {
    attrs = attr(data, "eyetrackingR")
    summarized_by = attrs$summarized_by
    if (is.null(summarized_by)) stop(test, " requires summarized data. ",
                                     "When using the 'time_shape' function, please select an argument for 'summarize_by'",
                                     " (e.g., the participant column).")
  } 
  df_analyze = data
  
  # auto-make a formula, unless they specified one
  if (is.null(formula)) {
    if (test=="lmer") stop("Must specify a formula if using lmer.")
    formula = as.formula(paste("Prop ~", condition_column))
  }
  
  # Run a model for each time-bin
  paired = list(...)[["paired"]]
  if (!quiet) message("Computing ", test, " for each time bin...")
  failsafe_test = failwith(default = NA, f = get(test), quiet = FALSE)
  if (quiet) pblapply = lapply
  models= pblapply(unique(df_analyze$TimeBin), function(tb) {
    # make model:
    temp_dat = filter(df_analyze, TimeBin==tb)
    
    # Makes paired test more robust to unpaired observations within a bin:
    if (identical(paired, TRUE)) {
      lvl1 = levels(data[[condition_column]])[1]
      temp_dat = try(
        temp_dat %>%
          group_by(ParticipantName) %>%
          mutate_(.dots = list(PairedObs = interp(~length(which(COND_COL == lvl1)) > 0 & length(which(COND_COL != lvl1)) > 0, 
                                                  COND_COL = as.name(condition_column))) ) %>%
          filter(PairedObs) %>%
          select(-PairedObs) %>%
          ungroup()
      )
    }
  
    model = failsafe_test(formula = formula, data = temp_dat, ... = ...) 
    # get N:
    if (test=="wilcox.test" | test=="lm") {
      condition_col_is_na = is.na(temp_dat[[condition_column]])
      model$sample_size = length(unique( temp_dat[[data_options$participant_column]][!condition_col_is_na] ))
    }
    model
  })
  
  # Get Statistic:
  if (test=="lmer") {
    tidied_models = lapply(models, tidy, effects="fixed")
  } else {
    tidied_models = lapply(models, tidy)
  }
  if (test %in% c('t.test','wilcox.test')) {
    models_statistics = sapply(tidied_models, function(x) ifelse('statistic' %in% names(x), x[,'statistic'], NA) )
  } else {
    models_statistics = sapply(tidied_models, function(x) {
      which_row = grep(pattern = condition_column, x = x[['term']], fixed = TRUE) # look for partially matching param (for treatment coding)
      if (length(which_row)==1) {
        return(x[which_row, 'statistic'])
      } else {
        # too many matches? look for exact match (happens with continous predictor)
        which_row = which(x[['term']] == condition_column)
        if (length(which_row)==1) return(x[which_row, 'statistic'])
        warning("Could not find the parameter '",condition_column,"' in your model. Found instead: ", paste(x[['term']], collapse=", ") )
        return(NA)
      }
    } )
  }
  
  
  # Find Critical Value:
  if (is.null(threshold)) {
    if (test == "lmer") {
      if (!quiet) message("Using the normal approximation for critical value on parameter in lmer.")
      crit_pos =  qnorm(p=1-alpha/2)
      crit_neg = -qnorm(p=1-alpha/2)
    } else if (test=="t.test") {
      dfs = sapply(tidied_models, function(x) ifelse('parameter' %in% names(x), x[,'parameter'], NA))
      crit_pos = qt(1-alpha/2, df = dfs)
      crit_neg = -crit_pos
    } else if (test=="wilcox.test") {
      sample_sizes = sapply(models, function(x) x$sample_size)
      crit_pos = qsignrank(p = 1-alpha/2, n = sample_sizes )
      crit_neg = -crit_pos
    } else if (test=="lm") {
      sample_sizes = sapply(models, function(x) x$sample_size)
      crit_pos = qt(1-alpha/2, df = sample_sizes-1)
      crit_neg = -crit_pos
    }
  } else {
    crit_pos = ifelse(sign(threshold)==1,  threshold, -threshold)
    crit_neg = ifelse(sign(threshold)==1, -threshold,  threshold)
  }
  
  # Return DataFrame:
  out = data.frame(stringsAsFactors = FALSE,
                   Statistic = models_statistics,
                   CritStatisticPos = crit_pos,     
                   CritStatisticNeg = crit_neg,     
                   TimeBin = unique(df_analyze$TimeBin)) # same order as for loop that built models
  out$AOI = df_analyze$AOI[1]
  if (return_model) out$Model = models
  
  out = as.data.frame(out)
  class(out) = c('bin_analysis', class(out))
  out
}

#' analyze_bootstraps()
#' 
#' Estimates a confidence interval over the difference between means (within- or between-subjects)
#' from a bootstrapped_shape object. Confidence intervals are derived from the alpha
#' used to shape the dataset (e.g., alpha = .05, CI=(.025,.975); alpha=.01, CI=(.005,.0995))
#' 
#' @param dataframe.bootstrapped_shape data The output of the 'bootstrapped_shape' function
#' ...
#' @return dataframe 
#' 
analyze_bootstraps = function(data, data_options) {
  UseMethod("analyze_bootstraps")
}
analyze_bootstraps.bootstrapped_shape <- function(data, data_options) {

  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(data, "eyetrackingR")
  bootstrap_attr = attrs$bootstrapped
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later
  
  # adjust CI based on alpha
  low_prob <- .5 - ((1-bootstrap_attr$alpha)/2)
  high_prob <- .5 + ((1-bootstrap_attr$alpha)/2)
  
  # if it's within subjects, getting the Mean and CI involves only taking the mean and 1.96*SD at each timepoint
  if (bootstrap_attr$within_subj == TRUE) {
    
    samples = data[, -1]

    bootstrapped_data <- data.frame(
      Time = data[['Time']],
      MeanDiff = apply(samples, 1, mean),
      SE = apply(samples, 1, sd),
      CI_low = round(apply(samples, 1, function (x) { quantile(x,probs=low_prob) }),5),
      CI_high = round(apply(samples, 1, function (x) { quantile(x,probs=high_prob) }),5)
    )
    
    bootstrapped_data <- mutate(bootstrapped_data,
                                Significant = ifelse((CI_high > 0 & CI_low > 0) | (CI_high < 0 & CI_low < 0), TRUE, FALSE))
  }
  else {
    samples <- bootstrap_attr$samples
    
    # randomly resample 1 mean from each condition and subtract them to get a
    # distribution of the difference between means
    bootstrapped_diffs <- data.frame(matrix(nrow=length(unique(data[, 'Time'])), ncol=bootstrap_attr$samples + 1))
    colnames(bootstrapped_diffs) <- c('Time', paste0('Diff',1:samples))
    
    get_mean_diffs <- function(samples, mean_dist_1, mean_dist_2) {
      return (sample(mean_dist_1,samples,replace=T) - sample(mean_dist_2,samples,replace=T))
    }
    
    # lay the 2 conditions side-by-side in a matrix
    horizontal_matrix <- cbind(data[1:(nrow(data)/2), c('Time',paste0('Sample',1:bootstrap_attr$samples))], data[((nrow(data)/2)+1):nrow(data), paste0('Sample',1:bootstrap_attr$samples)])
    
    # sample diffs between random means to generate a distribution of differences
    sampled_mean_diffs <- apply(horizontal_matrix, 1, function(x) { get_mean_diffs(1000, x[2:(length(x) / 2)], x[((length(x) / 2)+1):length(x)]) })
    sampled_mean_diffs <- t(sampled_mean_diffs)

    # calculate means and CIs
    bootstrapped_data <- data.frame(
      Time = horizontal_matrix$Time,
      MeanDiff = as.vector(apply(sampled_mean_diffs, 1, mean, na.rm=TRUE)),
      SE = as.vector(apply(sampled_mean_diffs, 1, sd, na.rm=TRUE)),
      CI_low = as.vector(round(apply(sampled_mean_diffs, 1, function (x) { quantile(x,probs=low_prob, na.rm=TRUE) }),5)),
      CI_high = as.vector(round(apply(sampled_mean_diffs, 1, function (x) { quantile(x,probs=high_prob, na.rm=TRUE) }),5))
    )
    
    bootstrapped_data <- bootstrapped_data %>%
      mutate(Significant = ifelse((CI_high > 0 & CI_low > 0) | (CI_high < 0 & CI_low < 0), TRUE, FALSE))
  }
  
  bootstrapped_data = as.data.frame(bootstrapped_data)
  class(bootstrapped_data) = c('bootstrapped_intervals_shape', class(bootstrapped_data))
  attr(bootstrapped_data, 'eyetrackingR') = list(bootstrapped = bootstrap_attr)
  
  return(bootstrapped_data)
}

#' analyze_bootstrapped_divergences()
#' 
#' TO DO: is this function really necessary?
#' 
#' Returns the windows in which the splines diverged in the bootstrapped analysis.
#' 
#' @param dataframe data Returned from analyze_bootstraps()
#' ...
#' @return dataframe 
#' 
analyze_bootstrapped_divergences = function(data, data_options) {
  UseMethod("analyze_bootstrapped_divergences")
}
analyze_bootstrapped_divergences.bootstrapped_intervals_shape <- function(data, data_options) {

  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(data, "eyetrackingR")
  bootstrap_attr = attrs$bootstrapped
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later
  
  # find divergences as runs of Significant == TRUE
  divergences <- rle(c(FALSE,data$Significant))
  
  if (sum(divergences$values) == 0) {
    return(NULL)
  }
  else {
    # convert to time ranges
    divergences$lengths <- (divergences$lengths * bootstrap_attr$resolution)
    divergences$timestamps <- cumsum(divergences$lengths) + bootstrap_attr$min_time-bootstrap_attr$resolution
    
    divergences <- paste0('divergence: ', divergences$timestamps[which(divergences$values == TRUE)-1], ' - ', divergences$timestamps[which(divergences$values == TRUE)])
    
    return(divergences)
  }
}

# Visualizing ------------------------------------------------------------------------------------------

# plot.data.frame()
#
# Prevents accidental plotting of non-analysis object
#
# @param dataframe data
# @param list data_options
# @param character condition_column
#
# @return an error

plot.data.frame <- function(data, data_options, condition_column) {
  stop("The class of this data, which specifies what type of data it is (its shape), has been removed.",
       "This can happen by using functions that transform the data significantly, such as dplyr's summarize and select.")
}


# plot.window_shape()
#
# Plots a window shape
#
# @param dataframe data returned by window_shape()
# @param list data_options
# @param character x_axis_column
# @param character group_column
#
# @return list A ggplot list object  
plot.window_shape <- function(data, data_options, x_axis_column, group_column = NULL) {
  
  dopts = data_options
  
  # Organize Vars:
  if ( !is.null(group_column) ) {
    if (is.numeric(data[[group_column]])) {
      message("The variable '", group_column, "' is continous, will perform median split for visualization.")
      split_col = paste0(group_column, "_Split")
      data[[split_col]] = ifelse(data[[group_column]] > median(data[[group_column]], na.rm=TRUE), 'High', 'Low')
      group_column = split_col
    }
  }
  color_var = group_column
  group_var = ifelse( is.null(group_column), 1, group_column)
  
  # Summarize by Participants:
  summarized = data %>%
    group_by_(.dots = c(dopts$participant_column, x_axis_column, group_column, "AOI")) %>%
    summarise(Prop = mean(Prop, na.rm= TRUE) ) %>%
    ungroup()
  
  # Plot:
  if ( is.numeric(summarized[[x_axis_column]]) ) {
    ggplot(summarized, aes_string(x = x_axis_column, y = "Prop", group= group_var, color= color_var)) +
      geom_point() +
      stat_smooth(method="lm") +
      facet_wrap( ~ AOI) +
      ylab("Proportion Looking")
  } else {
    ggplot(summarized, aes_string(x = x_axis_column, y = "Prop", group= group_var, color= color_var)) +
      geom_point() +
      stat_summary(fun.y = mean, geom='line') +
      stat_summary(fun.dat = mean_cl_boot) +
      facet_wrap( ~ AOI) +
      ylab("Proportion Looking")
  }
  
}

# plot.time_shape()
#
# Plot the timecourse of looking across groups. Median split if factor is continous
#
# @param dataframe data
# @param list data_options
# @param character condition_column
#
# @return list A ggplot list object  
plot.time_shape <- function(data, data_options, condition_column=NULL, dv='Prop') {
  require('ggplot2', quietly=TRUE)
  
  ## Prelims:
  data = ungroup(data)
  dopts = data_options
  
  ## Check condition factor
  if ( length(condition_column) > 1 ) {
    stop('Can only plot time-analysis for one factor at a time.')
  } 
  numeric_condition_col = FALSE
  if (!is.null(condition_column)) {
    if (is.numeric(data[[condition_column]])) numeric_condition_col = TRUE
  }
  
  ## Plot:
  if (numeric_condition_col) {
    message("Condition factor is numeric, performing median split...")
    data[["GroupFactor"]] = ifelse(data[[condition_column]] > median(data[[condition_column]], na.rm=TRUE), "High", "Low")

    g <- ggplot(out, aes_string(x = "Time", y=dv, group="GroupFactor", color="GroupFactor")) +
      stat_summary(fun.y='mean', geom='line') +
      stat_summary(fun.data='mean_cl_normal', geom='ribbon', mult=1, alpha=.2, colour=NA) +
      facet_wrap( ~ AOI) +
      guides(color= guide_legend(title= condition_column)) +
      xlab('Time (ms) in Trial')
    return(g)
    
  } else {
    g <- ggplot(data, aes_string(x = "Time", y=dv, group=condition_column, color=condition_column, fill=condition_column)) +
      stat_summary(fun.y='mean', geom='line') +
      stat_summary(fun.data='mean_cl_normal', geom='ribbon', mult=1, alpha=.2, colour=NA) +
      facet_wrap( ~ AOI) +
      xlab('Time (ms) in Trial')
    return(g)
  }
  
}


# plot.bin_analysis()
#
# Plot the result from the 'analyze_time_bins' function
#
# @param dataframe data
#
# @return list A ggplot list object  
plot.bin_analysis <- function(data) {
  require('ggplot2', quietly=TRUE)
  
  ggplot(data = data) +
    geom_line(mapping = aes(x = TimeBin, y= Statistic)) +
    geom_line(mapping = aes(x = TimeBin, y= CritStatisticPos), linetype="dashed") +
    geom_line(mapping = aes(x = TimeBin, y= CritStatisticNeg), linetype="dashed") +
    ylab("Statistic") +
    xlab("TimeBin") +
    facet_wrap( ~ AOI)
}


# plot.onset_shape()
#
# divide trials into which AOI they started on; plot proportion looking away from that AOI.
#
# @param dataframe.onset_shape data The output of the 'onset_shape' function
#...
# @return dataframe 

plot.onset_shape = function(data, data_options, condition_columns=NULL, smoothing_window_size = NULL) {
  require(ggplot2, quietly=TRUE)
  
  ## Prelims:
  if (length(condition_columns) > 2) {
    stop("Maximum two condition factors")
  }
  attrs = attr(data, "eyetrackingR")
  onset_attr = attrs$onset_contingent
  if (is.null(onset_attr)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  
  # set smoothing_window_size based on fixation_window_length in attr's
  if (is.null(smoothing_window_size)) {
    smoothing_window_size <- onset_attr$fixation_window_length
  }
  
  # clean out unknown first AOIs:
  df_clean = filter(data, !is.na(FirstAOI))
  for (condition_col in condition_columns) {
    df_clean = filter_(df_clean, 
                       .dots = list(interp(~!is.na(CONDITION_COL), CONDITION_COL = as.name(condition_col))
                       ))
  }
  
  # summarise by time bin:
  df_clean[[".Time"]] = floor(df_clean[[data_options$time_column]] / smoothing_window_size) * smoothing_window_size
  df_grouped = group_by_(df_clean, 
                         .dots = c(condition_columns, data_options$participant_column, ".Time", "FirstAOI", onset_attr$distractor_aoi, onset_attr$target_aoi)) 
  df_smoothed = summarise(df_grouped, SwitchAOI = mean(SwitchAOI, na.rm=TRUE))
  
  # collapse into lines for graphing:
  df_summarized = group_by_(df_smoothed, .dots = c(".Time", "FirstAOI", condition_columns) )
  df_summarized = summarise(df_summarized, SwitchAOI = mean(SwitchAOI, na.rm=TRUE))
  
  # compute grayed area:
  df_graph = group_by_(df_summarized, .dots = c(".Time", condition_columns) )
  df_graph = mutate(df_graph,
                    Max= max(SwitchAOI),
                    Min= min(SwitchAOI),
                    Top= ifelse(length(which(FirstAOI==onset_attr$distractor_aoi)), SwitchAOI[FirstAOI==onset_attr$distractor_aoi], 0) 
                    )
  df_graph$Max = with(df_graph, ifelse(Max==Top, Max, NA))
  df_graph$Min = with(df_graph, ifelse(Max==Top, Min, NA))
  
  ## Graph:
  if (is.null(condition_columns)) {
    color_factor= NULL
  } else {
    color_factor = condition_columns[1]
  }
  g = ggplot(df_graph, aes_string(x = ".Time", y = "SwitchAOI", 
                                  group = "FirstAOI", 
                                  color = color_factor)) +
    geom_line(size=1.5, aes(linetype=FirstAOI)) +
    geom_ribbon(aes(ymin= Min, ymax= Max), fill= "gray", alpha= .2, colour= NA) +
    coord_cartesian(xlim=c(onset_attr$onset_time, max(df_graph$.Time) )) +
    ylab("Proportion Switch Looking") + 
    xlab("Time") 
  
  ## Add Facets for Conditions:
  if (length(condition_columns)>1) return(g+facet_grid(as.formula(paste(condition_columns, collapse="~"))))
  if (length(condition_columns)>0) return(g+facet_grid(as.formula(paste(condition_columns, "~ ."))))
  return(g)
  
}

# plot.switch_shape()
#
# Boxplot of mean switch time aggregated by subjects within each FirstAOI, potentially faceted by condition_columns.
#
# @param dataframe.switch_shape data The output of the 'switch_shape' function
#...
# @return dataframe 

plot.switch_shape = function(data, data_options, condition_columns=NULL) {
  require(ggplot2, quietly=TRUE)
  
  ## Prelims:
  if (length(condition_columns) > 2) {
    stop("Maximum two condition factors")
  }
  
  ## Prepare for Graphing:
  out = data %>%
    group_by_(.dots = c(data_options$participant_column, condition_columns, "FirstAOI")) %>%
    summarise(MeanFirstSwitch = mean(FirstSwitch)) %>%
    ungroup()
  
  ## Graph:
  if (is.null(condition_columns)) {
    color_factor= NULL
  } else {
    color_factor = condition_columns[1]
  }
  
  g = ggplot(out, aes_string(x = "FirstAOI", y = "MeanFirstSwitch", 
                             color = color_factor)) +
    geom_boxplot() +
    geom_point(position = position_jitter(.1)) +
    coord_flip() +
    ylab("Mean Switch Time") + 
    xlab("Onset AOI") 
  
  ## Add Facets for Conditions:
  if (length(condition_columns)>1) return(g+facet_grid(as.formula(paste(condition_columns, collapse="~"))))
  if (length(condition_columns)>0) return(g+facet_grid(as.formula(paste(condition_columns, "~ ."))))
  
  return(g)
  
}

# plot.bootstrapped_shape()
#
# Plot the means and CIs of bootstrapped splines (either within-subjects or between-subjects)
#
# @param dataframe.bootstrapped_shape data The output of the 'bootstrapped_shape' function
#...
# @return dataframe 

plot.bootstrapped_shape = function(data, data_options) {
  require(ggplot2, quietly=TRUE)
  
  # Must be a bootstrapped_shape:
  if (!'bootstrapped_shape' %in% class(data)) stop('This function can only be run on the output of the "bootstrapped_shape" function.')
  
  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(data, "eyetrackingR")
  bootstrap_attr = attrs$bootstrapped
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later
  
  # if within-subjects, plot difference score
  if (bootstrap_attr$within_subj == TRUE) {
    # use plot.bootstrapped_intervals_shape() to plot within-subjects difference
    # because, for a within-subjects test, this is all that matters
    data <- analyze_bootstraps(data, data_options)
    
    return (plot(data, data_options))
  }
  else {
    data$Mean <- apply(data[, paste0('Sample',1:bootstrap_attr$samples)], 1, mean)
    data$SE <- apply(data[, paste0('Sample',1:bootstrap_attr$samples)], 1, sd)
    
    low_prob <- .5 - ((1-bootstrap_attr$alpha)/2)
    high_prob <- .5 + ((1-bootstrap_attr$alpha)/2)
    
    data$CI_high <- round(apply(data[, paste0('Sample',1:bootstrap_attr$samples)], 1, function (x) { quantile(x,probs=high_prob, na.rm=TRUE) }),5)
    data$CI_low <- round(apply(data[, paste0('Sample',1:bootstrap_attr$samples)], 1, function (x) { quantile(x,probs=low_prob, na.rm=TRUE) }),5)
    
    g <- ggplot(data, aes_string(x='Time', y='Mean', color=bootstrap_attr$condition_column)) +
      geom_line() +
      geom_ribbon(aes_string(ymax='CI_high', ymin='CI_low', fill=bootstrap_attr$condition_column), mult=1, alpha=.2, colour=NA) +
      xlab('Time') +
      ylab('Proportion')
  }
  
  g
}

# plot.bootstrapped_intervals()
#
# Plot the means and CIs of bootstrapped spline difference estimates and intervals
# (either within-subjects or between-subjects)
#
# @param dataframe.bootstrapped_intervals data The output of the 'analyze_bootstraps' function
#...
# @return dataframe 

plot.bootstrapped_intervals_shape = function(data, data_options) {
  require(ggplot2, quietly=TRUE)
  
  # Must be a bootstrapped_intervals_shape:
  if (!'bootstrapped_intervals_shape' %in% class(data)) stop('This function can only be run on the output of the "analyze_bootstraps_intervals" function.')
  
  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(data, "eyetrackingR")
  bootstrap_attr = attrs$bootstrapped
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later
  
  # we have a MeanDiff and CI for both within- and between-subjects...
  g <- ggplot(data, aes(x=Time, y=MeanDiff)) +
    geom_line() +
    geom_ribbon(aes(ymax=CI_high, ymin=CI_low), mult=1, alpha=.2, colour=NA) +
    xlab('Time')
  
  if (bootstrap_attr$within_subj == TRUE) {
    g <- g + ylab('Difference Score (within-subjects)')
  }
  else {
    g <- g + ylab('Difference Score (between-subjects)')
  }
  
  g
}

# Helpers -----------------------------------------------------------------------------------------------

# center_predictors()
#
# Center predictors in preparation for statistical analyses
#
# @param dataframe data
# @param character.vector predictors
#
# @return dataframe data with modified columns appended with "C"

center_predictors = function(data, predictors) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  mutate_argument = list()
  for (i in seq_along(predictors)) {
    name = paste0(predictors[i], "C")
    mutate_argument[[name]] = interp(~as.numeric(PREDICTOR) - mean(as.numeric(PREDICTOR), na.rm=TRUE), PREDICTOR = as.name(predictors[i]) )
  }
  
  data %>% 
    ungroup() %>%
    mutate_(.dots = mutate_argument)
  
}

#' .make_proportion_looking_summary()
#' 
#' A helper function for window_shape and time_shape. Takes a dataframe, groups it, and returns proportion looking
#' and relevant transformations
#' 
#' @param dataframe data
#' @param list groups
#' @param character aoi_col
#' 
#' @return dataframe
.make_proportion_looking_summary = function(data, groups, aoi_col) {
  # Group, Summarise Samples
  df_grouped = group_by_(data, .dots = groups)
  df_summarized = summarise_(df_grouped,
                             .dots = list(SamplesInAOI = interp(~sum(AOI_COL, na.rm=TRUE), AOI_COL = aoi_col),
                                          SamplesTotal = interp(~sum(!is.na(AOI_COL)), AOI_COL = aoi_col) # ignore all NAs
                             ))
  
  # Calculate Proportion, Elog, etc.
  aoi = as.character(aoi_col)
  out = mutate(df_summarized,
               AOI = aoi,
               Elog = log( (SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5) ),
               Weights = 1 / ( ( 1 / (SamplesInAOI + .5) ) / ( 1 / (SamplesTotal - SamplesInAOI +.5) ) ),
               Prop = SamplesInAOI / SamplesTotal,
               ArcSin = asin( sqrt( Prop ))
  )
  out = ungroup(out)
}


# Friendly Dplyr Verbs ----------------------------------------------------------------------------------
# dplyr verbs remove custom classes from dataframe, so a custom method needs to be written to avoid this

mutate_.time_shape = mutate_.window_shape = mutate_.bin_analysis = mutuate_.bootstrapped_shape = mutate_.bootstrapped_intervals_shape = mutate_.onset_shape = function(data, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bootstrapped_shape', 'bootstrapped_intervals_shape', 'bin_analysis')
  temp_remove = class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr = attr(data, "eyetrackingR") # also attributes
  
  out = mutate_(data, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

group_by_.time_shape = group_by_.window_shape = group_by_.bin_analysis = mutuate_.bootstrapped_shape = group_by_.bootstrapped_intervals_shape = group_by_.onset_shape = function(data, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bootstrapped_shape', 'bootstrapped_intervals_shape', 'bin_analysis')
  temp_remove = class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr = attr(data, "eyetrackingR") # also attributes
  
  out = group_by_(data, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

filter_.time_shape = filter_.window_shape = filter_.bin_analysis = filter_.bootstrapped_shape = filter_.bootstrapped_intervals_shape = filter_.onset_shape = function(data, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bootstrapped_shape', 'bootstrapped_intervals_shape', 'bin_analysis')
  temp_remove = class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr = attr(data, "eyetrackingR") # also attributes
  
  out = filter_(data, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

left_join.time_shape = left_join.window_shape = left_join.bin_analysis = left_join.bootstrapped_shape = left_join.bootstrapped_intervals_shape = left_join.onset_shape = function(x, y, by = NULL, copy = FALSE, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bootstrapped_shape', 'bootstrapped_intervals_shape', 'bin_analysis')
  temp_remove = class(x)[ class(x) %in% potential_classes]
  class(x) = class(x)[!class(x) %in% potential_classes]
  temp_attr = attr(x, "eyetrackingR") # also attributes
  
  out = left_join(x=x, y=y, by = by, copy = copy, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}


