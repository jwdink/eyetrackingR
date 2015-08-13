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
    summarise_(.dots = list(Mean = interp( ~mean(DV, na.rm=TRUE),     DV = dv ),
                            SD   = interp( ~sd(DV, na.rm=TRUE),       DV = dv ),
                            Var  = interp( ~var(DV, na.rm=TRUE),      DV = dv ),
                            Min  = interp( ~mean(DV, na.rm=TRUE)*1.0, DV = dv ),
                            Max  = interp( ~mean(DV, na.rm=TRUE)*1.0, DV = dv ),
                            NumTrials = interp( ~ n_distinct(TRIAL_COL), TRIAL_COL = data_options$trial_column)
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
                            TracklossForParticipant = mean(TracklossForParticipant, na.rm=TRUE)) %>%
    ungroup()
  
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
  data[[data_options$trackloss_column]] = ifelse(.AOISum == 0, TRUE, .data_no_na[[data_options$trackloss_column]])
  
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
#' @param logical delete_rows (default: FALSE) Should rows with trackloss be deleted?
#' 
#' @return dataframe 
remove_trackloss = function(data, data_options, delete_rows = FALSE) {
  
  data = ungroup(data)
  
  if (delete_rows) {
    
    # We want to only remove rows that are positively identified as trackloss, so we replace any trackloss=NA with trackloss=FALSE
    data[[".TracklossBoolean"]] = ifelse(is.na(data[[data_options$trackloss_column]]), FALSE, data[[data_options$trackloss_column]])

    # Remove all rows with Trackloss:
    out = filter(data, .TracklossBoolean == 0)
    
  } else {
    # Set Looking-at-AOI to NA for any samples where there is Trackloss:
    out = data
    for (aoi in data_options$aoi_columns) {
      out[[aoi]] = ifelse(data[[data_options$trackloss_column]]==1, NA, data[[aoi]] )
    }
  }
  
  out
}

# Shaping ------------------------------------------------------------------------------------------


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

#' window_shape()
#' 
#' Collapse time across our entire window and return a dataframe ready for analyses (e.g., lmer)
#' 
#' @param dataframe data
#' @param list data_options
#' @param string aoi                            Which AOIs are of interest? Defaults to all in 'data_options'
#' @param character.vector condition_columns    Which columns indicate conditions, and therefore should be 
#'                                              preserved in grouping operations?
#' 
#' @return dataframe
window_shape <- function(data, 
                         data_options, 
                         aoi = data_options$aoi_columns, 
                         condition_columns = NULL
) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  # For Multiple AOIs:
  if (length(aoi) > 1) {
    list_of_dfs = lapply(X = aoi, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      window_shape(data, data_options, aoi = this_aoi, condition_columns)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('window_shape', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data)
  aoi_col = as.name(aoi)

  # Make Summary
  out = .make_proportion_looking_summary(data=data, groups = list(data_options$participant_column, data_options$trial_column), aoi_col)
               
  class(out) = c('window_shape', class(out))
  return(out)
  
}

#' time_shape()
#' 
#' Creates time-bins and summarizes proportion-looking within each time-bin
#' 
#' @param dataframe data
#' @param list data_options
#' @param integer time_bin_size  The time to fit into each bin
#' @param character              aoi Which AOI(s) do you want to analyze?
#' @param character.vector       condition_columns 
#' 
#' @return dataframe summarized

time_shape <- function (data, 
                        data_options, 
                        time_bin_size, 
                        aoi = data_options$aoi_columns, 
                        condition_columns = NULL) {
  
  
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  
  # For Multiple AOIs:
  if (length(aoi) > 1) {
    list_of_dfs = lapply(X = aoi, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      time_shape(data, data_options, time_bin_size, this_aoi, condition_columns)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('time_shape', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data)
  aoi_col = as.name(aoi)
  
  # Make Time Bin:
  data[["TimeBin"]] = floor(data[[data_options$time_column]] / time_bin_size)
  
  # Make Summary
  df_summarized = .make_proportion_looking_summary(data=data, 
                                                   groups = list(data_options$participant_column, data_options$trial_column, "TimeBin"), 
                                                   aoi_col)
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
  colnames(time_codes) <- c('TimeBin',paste0("OT", 1:max_degree))
  
  out <- left_join(df_summarized, time_codes, by='TimeBin')
  
  class(out) = c('time_shape', class(out))
  
  return(out)
  
}


#' onset_shape()
#' 
#' divide trials into which AOI they started on; augment with column indicating switch away from that AOI
#' 
#' @param dataframe data            The original (verified) data
#' @param list data_options
#' @param numeric onset_time        When should we check for their "starting" AOI? 
#' @param numeric fixation_wind_len       Smoothes the data by determining the fixated AOI using a moving average over time bins that are this long
#'                                             (e.g., "100" means that the fixated AOI is determined over a 100ms rolling window)
#' @param character target_aoi      Which AOI is the target that should be switched *to*
#' @param character distractor_aoi  Which AOI is the distractor that should be switched *from* (default = !target_aoi)
#' @return dataframe 

onset_shape = function(data, data_options, onset_time, fixation_wind_len, target_aoi, distractor_aoi = NULL, condition_columns = NULL) {
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  require("zoo", quietly=TRUE)
  
  ## Helper Function:
  na_replace_rollmean = function(col) {
    col = ifelse(is.na(col), 0, col)
    rollmean(col, k = fixation_wind_len_rows, partial=TRUE, fill= NA, align="left")
  }
  
  ## Prelims:
  if (is.null(distractor_aoi)) {
    distractor_aoi = paste0("NOT_", target_aoi)
    data[[distractor_aoi]] = !data[[target_aoi]]
  }
  time_col = as.name(data_options$time_column)
  
  ## Translate TimeWindow units from time to number of rows (for rolling mean):
  df_time_per_row = group_by_(data, .dots = c(data_options$participant_column, data_options$trial_column, condition_columns) )
  df_time_per_row = summarise_(df_time_per_row,
                               .dots = list(TimePerRow = interp(~mean(diff(TIME_COL)), TIME_COL = time_col)
                                            ))
  time_per_row = round(mean(df_time_per_row[["TimePerRow"]]))
  fixation_wind_len_rows = fixation_wind_len / time_per_row
  
  ## Determine First AOI, Assign Switch Value for each timepoint
  
  # Group by Ppt*Trial:
  df_grouped = group_by( .dots = list(data_options$participant_column, data_options$trial_column) )
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
         FirstAOI  = ifelse(abs(.ClosestTime-onset_time) > fixation_wind_len, NA, FirstAOI),
         WhichAOI  = ifelse(.Target > .Distractor, target_aoi, distractor_aoi),
         SwitchAOI = FirstAOI != WhichAOI)
  out = select(out, -.Target, -.Distractor, -.Time, -.ClosestTime)

  # Assign class information:
  class(out) = c('onset_shape', class(out))
  attr(out, 'onset_contingent') = list(distractor_aoi = distractor_aoi, 
                                       target_aoi = target_aoi, 
                                       onset_time = onset_time,
                                       fixation_wind_len = fixation_wind_len)
  
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

switch_shape = function(data, data_options, condition_columns=NULL) {
  # Must be an onset_shape:
  if (!'onset_shape' %in% class(data)) stop('This function can only be run on the output of the "onset_shape" function.')
  
  dopts = data_options
  
  out <- data %>%
    filter(!is.na(FirstAOI)) %>%
    group_by_(.dots = as.list(c(dopts$participant_column, dopts$trial_column, dopts$item_columns, "FirstAOI", condition_columns))  ) %>%
    summarise_(.dots = list(
      FirstSwitch = make_dplyr_argument(dopts$time_column,"[first(which(SwitchAOI), order_by=", dopts$time_column, ")]-first(",dopts$time_column,")")
    ))
  
  class(out) = c('switch_shape', class(out))
  
  return(out)
}

# bootstrapped_shape(data, data_options, condition_column, within_subj, samples, resolution, alpha)
#
# Bootstrap splines from a time_shape() shape. Return bootstrapped splines.
#
# @param dataframe data Your clean dataset
# @param list data_options Standard list of options for manipulating dataset
# @param string factor What factor to split by? Maximum two conditions!
# @param boolean within_subj Are the two conditions within or between subjects?
# @param int samples How many (re)samples to take?
# @param float resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100 intervals per second, or hundredths of a second
# @param float alpha p-value when the groups are sufficiently "diverged"
# @param string smoother Smooth data using "smooth.spline," "loess," or leave NULL for no smoothing 
# 
# @return list(samples, divergence)
bootstrapped_shape <- function (data, data_options, condition_column, within_subj = F, samples = 1000, resolution = 10, alpha = .05, smoother = 'none') {
  require(dplyr, quietly=T)
  require(reshape2, quietly=T)
  
  # validate arguments
  if (!condition_column %in% colnames(data)) {
    stop("bootstrapped_shape failed to find data in condition_column")
  }
  
  if ( length(levels(as.factor(data[[condition_column]]))) != 2 ) {
    stop('bootstrapped_shape requires a condition_column with 2 levels.')
  }
  
  if (!(smoother %in% c('smooth.spline','loess','none'))) {
    stop('bootstrapped_shape requires that "smoother" be set to "none", "smooth.spline", or "loess")')
  }
  
  # define sampler/bootstrapper:
  sampler <- function (dataframe, data_options, resolution, smoother) {
    # show a . everyone 10th sample to indicate progress
    if (rbinom(1,1,.1) == 1) {
      cat('.')
    }
    
    run_original <- dataframe
    
    # get subjects
    run_subjects <- levels(run_original[, data_options$participant_column])
    
    # get timepoints
    run_times <- unique(run_original$Time)
    run_times <- run_times[order(run_times)]
    
    # randomly sample N subjects (with replacement from data)
    run_sampled <- sample(run_subjects, length(run_subjects), replace = T)
    
    # create a dataset of ParticipantName,Time,Prop for each sampled subject (including duplicates)
    run_rows <- length(run_sampled) * length(run_times)
    run_data <- data.frame(matrix(nrow=run_rows,ncol=2))
    
    colnames(run_data) <- c(data_options$participant_column,'Time')
    
    run_data[, data_options$participant_column] <- rep(run_sampled, each=length(run_times))
    run_data[, data_options$participant_column] <- factor(run_data[, data_options$participant_column])
    
    run_data$Time <- rep(run_times, times=length(run_subjects))
    
    run_data <- inner_join(run_data, run_original[, c(data_options$participant_column,'Time','Prop')], by = c(data_options$participant_column,'Time'))
    
    if (smoother == "none") {
      # use straight linear approximation on the values
      run_predicted_times <- seq(min(run_times), max(run_times), by=resolution)
      
      run_predictions <- with(run_data,
                              approx(run_data$Time, run_data$Prop, xout=run_predicted_times))
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
  data[, data_options$participant_column] <- factor(data[,  data_options$participant_column])
  
  # between-subjects:
  if (within_subj == FALSE) {
    for (level in levels(factor(data[, condition_column]))) {
      subsetted_data <- data[which(data[, condition_column] == level), ]
      
      cat(paste0('Sampling ', level))
      bootstrapped_data <- replicate(samples, sampler(subsetted_data, data_options, resolution, smoother))
      bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=F))
      
      # label each sample by number
      sample_rows <- paste('Sample', c(1:samples), sep="")
      colnames(bootstrapped_data) <- sample_rows
      
      bootstrapped_data <- data.frame(
        condition_column = level,
        Time = seq(min(subsetted_data$Time), max(subsetted_data$Time), by=resolution),
        bootstrapped_data
      )
      colnames(bootstrapped_data)[1] <- condition_column
      
      # using dplyr's rbind_list or speed increase
      combined_bootstrapped_data <- rbind(combined_bootstrapped_data,bootstrapped_data)
    }
  }
  else {
    # within-subjects:
    
    # for within-subjects, we need to calculate the difference between
    # level 1 and 2 of the factor for each subject before sampling splines
    data <- dcast(data, as.formula(paste(paste(data_options$participant_column,'Time',sep=" + "),condition_column,sep=' ~ ')), value.var='Prop', fun.aggregate = mean, drop = TRUE)
    
    # re-calculate Prop as the DIFFERENCE between the two labels
    data$Prop <- data[, 3] - data[, 4]
    
    # remove all samples where Prop == NA
    data <- data[!is.na(data$Prop), ]
    
    data[, data_options$participant_column] <- factor(data[, data_options$participant_column])
    
    cat('Sampling within-subjects')
    bootstrapped_data <- replicate(samples, sampler(data, data_options, resolution, smoother))
    bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=F))
    
    sample_rows <- paste('Sample', c(1:samples), sep="")
    colnames(bootstrapped_data) <- sample_rows
    
    bootstrapped_data <- data.frame(
      Time = seq(min(data$Time), max(data$Time), by=resolution),
      bootstrapped_data
    )
    
    # use dplyr's rbind_list for speed increase
    combined_bootstrapped_data <- rbind(combined_bootstrapped_data,bootstrapped_data)
  }
  
  # Assign class information:
  class(combined_bootstrapped_data) = c('bootstrapped_shape', class(combined_bootstrapped_data))
  attr(combined_bootstrapped_data, 'bootstrapped') = list(
    within_subj = within_subj,
    condition_column = condition_column,
    samples = samples,
    alpha = alpha,
    resolution = resolution,
    min_time = min(combined_bootstrapped_data[, 'Time'])
  )
  
  return(combined_bootstrapped_data)
}

# Analyzing ------------------------------------------------------------------------------------------

analyze_clusters = function(data, data_options, condition_column, method, alpha, ...) {
  UseMethod("analyze_clusters")
}

#' analyze_clusters.time_shape()
#' 
#' Takes data that has been summarized into time-bins, and finds adjacent time bins that
#' pass some threshold of significance, and assigns these adjacent groups into clusters
#' for further examination.
#' 
#' @param dataframe.time_shape data The output of the 'time_shape' function
#' @param list data_options            
#' ...
#' @return dataframe 

analyze_clusters.time_shape = function(data, data_options, condition_column, paired=FALSE, alpha = .10) {
  
  ## Helper:
  label_clusters = function(vec) {
    vec[is.na(vec)] = 0
    out = c(cumsum(diff(vec)==1))
    out[!vec] = NA
    out
  }
  
  ## Test Bins:
  time_bin_summary = analyze_time_bins(data, data_options, condition_column, paired=paired, alpha = alpha)
  
  #   ## Label Adjacent Clusters:
  #   time_bin_summary$Sig = time_bin_summary$Statistic > time_bin_summary$CritStatistic
  #   time_bin_summary %>%
  #     group_by(AOI) %>%
  #     mutate(Cluster = label_clusters(Sig))
  #   
  #   cat("")
}


#' analyze_time_bins()
#' 
#' Runs a test on each time-bin of a time-analysis. Defaults to a t-test, but supports wilcox, lm, and lmer as well.
#' 
#' @param dataframe.time_shape data   The output of the 'time_shape' function
#' @param list data_options
#' @param character condition_column  The variable whose test statistic you are interested in
#' ...
#' @return dataframe 

analyze_time_bins <- function(data, 
                              data_options, 
                              condition_column,
                              threshold = NULL,
                              alpha = .05,
                              test = "t.test",
                              formula = NULL,
                              return_model = FALSE,
                              ...)
{
  
  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)
  require('broom', quietly=TRUE)
  
  # Must be a time_shape:
  if (!'time_shape' %in% class(data)) stop('This function can only be run on the output of the "time_shape" function.')
  
  # For Multiple aois:
  aois = unique(data[['AOI']])
  if ( length(aois) > 1 ) {
    list_of_dfs = lapply(X = aois, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      this_df = filter(data, AOI == this_aoi)
      class(this_df) = class(data)
      analyze_time_bins(data = this_df, data_options, condition_column, threshold, alpha, test, formula, return_model, ...)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('bin_analysis', class(out))
    return( out )
  }
  
  # Collapse by participants if necessary:
  if (test != "lmer") {
    message("Collapsing data by participant...")
    df_grouped = group_by_(.data = data, .dots = list(data_options$participant_column, "TimeBin", "AOI", condition_column))
    df_by_part = summarise(.data = df_grouped, Prop = mean(Prop, na.rm=TRUE))
    df_analyze = ungroup(df_by_part)
  } else {
    df_analyze = data
  }
  
  # auto-make a formula, unless they specified one
  if (is.null(formula)) {
    if (test=="lmer") stop("Must specify a formula if using lmer.")
    formula = as.formula(paste("Prop ~", condition_column))
  }
  
  # Run a model for each time-bin
  message("Computing ", test, " for each time bin...")
  failsafe_test = failwith(default = NA, f = get(test), quiet = FALSE)
  models = list()
  samp_sizes = c()
  for (tb in unique(df_analyze$TimeBin)) {
    temp_dat = filter(df_analyze, TimeBin==tb)
    condition_col_is_na = is.na(temp_dat[[condition_column]])
    samp_sizes[as.character(tb)] = length(unique( temp_dat[[data_options$participant_column]][!condition_col_is_na] ))
    models[[as.character(tb)]] = failsafe_test(formula = formula, data = temp_dat, ... = ...)
  }
  
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
      cat("\nUsing the normal approximation for critical value on parameter in lmer.")
      crit_pos =  qnorm(p=1-alpha/2)
      crit_neg = -qnorm(p=1-alpha/2)
    } else if (test=="t.test") {
      dfs = sapply(tidied_models, function(x) ifelse('parameter' %in% names(x), x[,'statistic'], NA))
      crit_pos = qt(1-alpha/2, df = dfs)
      crit_neg = -crit_pos
    } else if (test=="wilcox.test") {
      crit_pos = qsignrank(p = 1-alpha/2, n = samp_sizes )
      crit_neg = -crit_pos
    } else if (test=="lm") {
      crit_pos = qt(1-alpha/2, df = samp_sizes-1)
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
                   TimeBin = unique(df_analyze$TimeBin), # same order as for loop that built models
                   AOI = df_analyze$AOI[1]) # all same
  if (return_model) out$Model = models
  
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
analyze_bootstraps <- function(data, data_options) {
  # Must be a bootstrapped_shape:
  if (!'bootstrapped_shape' %in% class(data)) stop('This function can only be run on the output of the "bootstrapped_shape" function.')
  
  # make sure there is the proper kind of data frame, and check its attributes
  bootstrap_attr = attr(data, "bootstrapped")
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later
  
  # adjust CI based on alpha
  low_prob <- .5 - ((1-bootstrap_attr$alpha)/2)
  high_prob <- .5 + ((1-bootstrap_attr$alpha)/2)
  
  # if it's within subjects, getting the Mean and CI involves only taking the mean and 1.96*SD at each timepoint
  if (bootstrap_attr$within_subj == TRUE) {
    # define range of sample rows
    sample_rows <- c(2:(2+bootstrap_attr$samples-1))
    
    bootstrapped_data <- data.frame(
      Time = data[, 'Time'],
      MeanDiff = apply(data[, sample_rows], 1, mean),
      SE = apply(data[, sample_rows], 1, sd),
      CI_low = round(apply(data[, sample_rows], 1, function (x) { quantile(x,probs=low_prob) }),5),
      CI_high = round(apply(data[, sample_rows], 1, function (x) { quantile(x,probs=high_prob) }),5)
    )
    
    bootstrapped_data <- bootstrapped_data %>%
      mutate(Significant = ifelse((abs(CI_high) - abs(CI_low)) == (CI_high - CI_low), TRUE, FALSE))
  }
  else {
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
      Time = unique(data[, 'Time']),
      MeanDiff = as.vector(apply(sampled_mean_diffs, 1, mean)),
      SE = as.vector(apply(sampled_mean_diffs, 1, sd)),
      CI_low = as.vector(round(apply(sampled_mean_diffs, 1, function (x) { quantile(x,probs=low_prob) }),5)),
      CI_high = as.vector(round(apply(sampled_mean_diffs, 1, function (x) { quantile(x,probs=high_prob) }),5))
    )
    
    bootstrapped_data <- bootstrapped_data %>%
      mutate(Significant = ifelse((abs(CI_high) - abs(CI_low)) == (CI_high - CI_low), TRUE, FALSE))
  }
  
  class(bootstrapped_data) = c('bootstrapped_intervals_shape', class(bootstrapped_data))
  attr(bootstrapped_data, 'bootstrapped') = bootstrap_attr
  
  return(bootstrapped_data)
}

#' analyze_bootstrapped_divergences()
#' 
#' Returns the windows in which the splines diverged in the bootstrapped analysis.
#' 
#' @param dataframe data Returned from analyze_bootstraps()
#' ...
#' @return dataframe 
#' 
analyze_bootstrapped_divergences <- function(data, data_options) {
  # Must be a bootstrapped_shape:
  if (!'bootstrapped_intervals_shape' %in% class(data)) stop('This function can only be run on the output of the "analyze_bootstrapped_intervals" function.')
  
  # make sure there is the proper kind of data frame, and check its attributes
  bootstrap_attr = attr(data, "bootstrapped")
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later
  
  # find divergences as runs of Significant == TRUE
  divergences <- rle(data$Significant)
  
  if (sum(divergences$values) == 0) {
    NULL
  }
  else {
    # convert to time ranges
    divergences$lengths <- (divergences$lengths * bootstrap_attr$resolution)
    divergences$timestamps <- cumsum(divergences$lengths) + bootstrap_attr$min_time
    
    divergences <- paste0('divergence: ', divergences$timestamps[which(divergences$values == TRUE)-1], ' - ', divergences$timestamps[which(divergences$values == TRUE)])
    
    divergences
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
  stop('Whoops, you have attempted to plot a dataframe, but this can only be done to dataframes that have ',
       'been produced by one of the "analysis" functions. If the dataframe you are attempting to plot was',
       'produced in this way, [THIS FUNCTION WILL BE A HELPER TO FIX CLASS].')
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
    median_split_arg = list(GroupFactor = 
                              make_dplyr_argument("ifelse(", condition_column, ">median(", condition_column, ", na.rm=TRUE), 'High', 'Low')" )
    )
    out = data %>%
      mutate_(.dots = median_split_arg)
    
    g <-  ggplot(out, aes_string(x = "Time", y=dv, group="GroupFactor", color="GroupFactor")) +
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
# this is NOT to be confused with the plot method for an onset_contingent dataframe.
#
# @param dataframe.onset_shape data The output of the 'onset_shape' function
#...
# @return dataframe 

plot.onset_shape = function(data, data_options, condition_columns=NULL) {
  require(ggplot2, quietly=TRUE)
  
  ## Prelims:
  if (length(condition_columns) > 2) {
    stop("Maximum two condition factors")
  }
  onset_attr = attr(data, "onset_contingent")
  if (is.null(onset_attr)) stop("Dataframe has been corrupted.") # <----- fix later
  
  # Brock: set smoothing_window_size based on fixation_wind_len in attr's
  # TODO: any problem with this?
  smoothing_window_size <- onset_attr$fixation_wind_len
  
  ## Prepare for Graphing:
  out = data %>%
    filter(!is.na(FirstAOI)) %>%
    # aggregate by participant, first
    group_by_(.dots = c(data_options$participant_column, data_options$time_column, "FirstAOI", onset_attr$distractor_aoi, onset_attr$target_aoi)) %>%
    summarise(SwitchAOI = mean(SwitchAOI, na.rm=TRUE)) %>%
    ungroup() %>%
    # now calculate mean proportion of switches over time
    mutate_(.dots = list(.Time = make_dplyr_argument("floor(", data_options$time_column, "/smoothing_window_size)*smoothing_window_size" )))  %>%
    group_by_(.dots = c(".Time", condition_columns, "FirstAOI")) %>%
    summarise(SwitchAOI = mean(SwitchAOI, na.rm=TRUE)) %>%
    mutate(Max= max(SwitchAOI),
           Min= min(SwitchAOI),
           Top= SwitchAOI[FirstAOI==onset_attr$distractor_aoi] )
  
  out$Max = with(out, ifelse(Max==Top, Max, NA))
  out$Min = with(out, ifelse(Max==Top, Min, NA))
  
  ## Graph:
  if (is.null(condition_columns)) {
    color_factor= NULL
  } else {
    color_factor = condition_columns[1]
  }
  g = ggplot(out, aes_string(x = ".Time", y = "SwitchAOI", 
                             group = "FirstAOI", 
                             color = color_factor)) +
    geom_line(size=1.5, aes(linetype=FirstAOI)) +
    geom_ribbon(aes(ymin= Min, ymax= Max), fill= "gray", alpha= .2, colour= NA) +
    coord_cartesian(xlim=c(onset_attr$onset_time, max(out$.Time) )) +
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
  bootstrap_attr = attr(data, "bootstrapped")
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
    
    data$CI_high <- round(apply(data[, paste0('Sample',1:bootstrap_attr$samples)], 1, function (x) { quantile(x,probs=high_prob) }),5)
    data$CI_low <- round(apply(data[, paste0('Sample',1:bootstrap_attr$samples)], 1, function (x) { quantile(x,probs=low_prob) }),5)
    
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
  bootstrap_attr = attr(data, "bootstrapped")
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
    mutate_argument[[name]] = make_dplyr_argument("as.numeric(", predictors[i], ") - mean(as.numeric(",  predictors[i], "), na.rm=TRUE)" )
  }
  
  data %>% 
    ungroup() %>%
    mutate_(.dots = mutate_argument)
  
}


# Friendly Dplyr Verbs ----------------------------------------------------------------------------------
# dplyr verbs remove custom classes from dataframe, so a custom method needs to be written to avoid this

mutate_.time_shape = mutate_.window_shape = mutate_.bin_analysis = mutuate_.bootstrapped_shape = mutate_.bootstrapped_intervals_shape = mutate_.onset_shape = function(data, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bootstrapped_shape', 'bootstrapped_intervals_shape', 'bin_analysis')
  temp_remove = class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr = attr(data, "onset_contingent") # also attributes
  
  out = mutate_(data, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  if ("onset_shape" %in% temp_remove) attr(out, "onset_contingent") = temp_attr
  
  return(out)
}

filter_.time_shape = filter_.window_shape = filter_.bin_analysis = filter_.bootstrapped_shape = filter_.bootstrapped_intervals_shape = filter_.onset_shape = function(data, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bootstrapped_shape', 'bootstrapped_intervals_shape', 'bin_analysis')
  temp_remove = class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr = attr(data, "onset_contingent") # also attributes
  
  out = filter_(data, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  if ("onset_shape" %in% temp_remove) attr(out, "onset_contingent") = temp_attr
  
  return(out)
}

left_join.time_shape = left_join.window_shape = left_join.bin_analysis = left_join.bootstrapped_shape = left_join.bootstrapped_intervals_shape = left_join.onset_shape = function(x, y, by = NULL, copy = FALSE, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bootstrapped_shape', 'bootstrapped_intervals_shape', 'bin_analysis')
  temp_remove = class(x)[ class(x) %in% potential_classes]
  class(x) = class(x)[!class(x) %in% potential_classes]
  temp_attr = attr(x, "onset_contingent") # also attributes
  
  out = left_join(x=x, y=y, by = by, copy = copy, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  if ("onset_shape" %in% temp_remove) attr(out, "onset_contingent") = temp_attr
  
  return(out)
}

# [ TO DO ]

