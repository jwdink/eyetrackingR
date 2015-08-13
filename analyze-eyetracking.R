# analyze-eyetracking.R
#
# This library helps prepare data for several different types of eyetracking analyses:
#   * Bin analyses
#   * Window analyses
#   * Linear timecourse analyses
#   * Non-linear (growth curve) timecourse analyses
#
# It also includes several functions for cleaning the dataset before each of these analyses.
#
#
# @author Brock Ferguson
#         brock.ferguson@gmail.com
#         brockferguson.com
#
# @author Jacob Dink
#         jacobwdink@gmail.com
#         github.com/jwdink
#
# @created July, 2012
# @re-written April, 2015


# Loading/Cleaning/Describing ------------------------------------------------------------------------

# set_data_options
#
# Create a list of data options which is passed to most of these
# methods.
#
# @param string participant_column Set to the subject code of the participant
# @param string trackloss_column Set to 1 or 0 depending on whether the sample is lost to trackloss
# @param string time_column The factor to use for judgments of time (ms)
# @param string trial_column The unique name of the current trial
# @param string/vector item_columns One or more columns that specify item/stimulus properties (values do not need to be unique, and this can be the same as trial)
# @param string/vector aoi_columns Names of all AOI columns (which should be logical, 1/0)

#
# @return list of configuration options

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

# verify_dataset()
#
# Verify, and fix, the status of the dataset by assessing columns in your data_options. 
#
# @param dataframe data
# @param list data_options
# @param boolean silent
#
# @return dataframe data

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

# subset_by_window ()
#
# Extract a subset of the dataset, where each trial falls inside a time-window.
# Time-window can either be specifed by a number (for a timestamp across all trials) or
# by a column which picks out a timestamp for each participant/trial
#
# @param dataframe data
# @param list data_options
# @param numeric/character window_start Number (for timestamp) or character (for column that specifies timestamp)
# @param numeric/character window_end Number (for timestamp) or character (for column that specifies timestamp)
# @param logical rezero Should the beginning of the window be considered the zero point of the timestamp? 
#                       Default TRUE when window_start is column, FALSE when window_start is number
#
# @return dataframe 

subset_by_window = function(data, data_options, window_start = -Inf, window_end = Inf, rezero = NULL) {
  require(dplyr, quietly=TRUE)
  
  dopts = data_options
  
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
  out = data %>%
    filter_(.dots = list(make_dplyr_argument(dopts$time_column, "> .WindowStart"),
                         make_dplyr_argument(dopts$time_column, "< .WindowEnd")
    ))
    
  # Rezero
  if (rezero) {
    out = out %>% 
      group_by_(.dots = list(dopts$participant_column, dopts$trial_column)) %>%
      mutate_(.dots = list(NewTimeStamp = make_dplyr_argument(dopts$time_column, "- .WindowStart"))) %>%
      ungroup()
    out[[dopts$time_column]] = out[["NewTimeStamp"]]
    out[["NewTimeStamp"]] = NULL
  }
  
  out[[".WindowStart"]] = NULL
  out[[".WindowEnd"]] = NULL
  
  out
}


# describe_data ()
#
# Describe a DV column in the dataset by a (group of) factor(s)
#
# @param dataframe data
# @param list data_options
# @param character dv
# @param character.vector factors
#
# @return dataframe 

describe_data = function(data, data_options, dv, factors) {
  require(dplyr, quietly=TRUE)
  
  data %>%
    group_by_(.dots = as.list(factors)) %>%
    summarise_(.dots = list(Mean = make_dplyr_argument( "mean(",dv,", na.rm=TRUE)" ),
                            SD   = make_dplyr_argument( "sd(",dv,", na.rm=TRUE)" ),
                            Var  = make_dplyr_argument( "var(",dv,", na.rm=TRUE)" ),
                            Min  = make_dplyr_argument( "min(",dv,", na.rm=TRUE)*1.0" ),
                            Max  = make_dplyr_argument( "max(",dv,", na.rm=TRUE)*1.0" ),
                            NumTrials = make_dplyr_argument( "length(unique(", data_options$trial_column, "))" )
    ))
  
}


# trackloss_analysis ()
#
# Get information on trackloss
#
# @param dataframe data
# @param list data_options
# @param numeric/character window_start Number (for timestamp) or character (for column that specifies timestamp)
# @param numeric/character window_end Number (for timestamp) or character (for column that specifies timestamp)
#
# @return dataframe 

trackloss_analysis = function(data, data_options, window_start = -Inf, window_end = Inf) {
  .zscore = function(x) (x-mean(x,na.rm=TRUE)) / sd(x,na.rm=TRUE)
  require('dplyr', quietly=TRUE)
  
  # Filter by Time-Window:
  subset_by_window(data, data_options, window_start, window_end) %>%
    # Get Trackloss-by-Trial:
    group_by_(.dots = list(data_options$participant_column, data_options$trial_column)) %>%
    mutate_(.dots = list(SumTracklossForTrial = make_dplyr_argument("sum(", data_options$trackloss_column, ", na.rm=TRUE)"),
                         TotalTrialLength = make_dplyr_argument("length(", data_options$trackloss_column, ")"),
                         TracklossForTrial = make_dplyr_argument("SumTracklossForTrial / TotalTrialLength")) ) %>%
    ungroup() %>%
    # Get Trackloss-by-Participant:
    group_by_(.dots = list(data_options$participant_column)) %>%
    mutate_(.dots = list(SumTracklossForParticipant = make_dplyr_argument("sum(", data_options$trackloss_column, ", na.rm=TRUE)"),
                         TotalParticipantLength = make_dplyr_argument("length(", data_options$trackloss_column, ")"),
                         TracklossForParticipant = make_dplyr_argument("SumTracklossForParticipant / TotalParticipantLength"))) %>%
    ungroup() %>%
    # Get Z-Scores:
    group_by_(.dots = list(data_options$participant_column, data_options$trial_column) )%>%
    summarise(Samples = mean(TotalTrialLength, na.rm=TRUE),
              TracklossSamples = mean(SumTracklossForTrial, na.rm=TRUE),
              TracklossForTrial = mean(TracklossForTrial, na.rm=TRUE),
              TracklossForParticipant = mean(TracklossForParticipant, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Part_ZScore  = .zscore(TracklossForParticipant),
           Trial_ZScore = .zscore(TracklossForTrial) )
}

# clean_by_trackloss ()
#
# Remove trials/participants with too much trackloss, with a customizable threshold
#
# @param dataframe data
# @param list data_options
# @param numeric participant_z_thresh Maximum amount of trackloss for participants, in terms of z-scores
# @param numeric trial_z_thresh Maxmimum amount of trackloss for trials, in terms of z-scores
# @param numeric participant_prop_thresh Maximum proportion of trackloss for participants
# @param numeric trial_prop_thresh Maximum proportion of trackloss for trials
# @param numeric/character window_start Number (for timestamp) or character (for column that specifies timestamp)
# @param numeric/character window_end Number (for timestamp) or character (for column that specifies timestamp)
#
# @return dataframe 

clean_by_trackloss = function(data, data_options, 
                              participant_z_thresh = Inf, trial_z_thresh = Inf, 
                              participant_prop_thresh = 1, trial_prop_thresh = 1,
                              window_start = -Inf, window_end = Inf) {
  data$.TrialID = paste(data[[data_options$participant_col]], data[[data_options$trial_col]], sep = "_")
  
  # Trackloss Analysis:
  message("Performing Trackloss Analysis...")
  tl = trackloss_analysis(data, data_options, window_start, window_end)
  
  # Bad Trials:
  if (trial_z_thresh < Inf) {
    message("Will exclude trials whose trackloss-z-score is greater than : ", trial_z_thresh)
    prop_thresh_trial = sd(tl$TracklossForTrial)*trial_z_thresh + mean(tl$TracklossForTrial)
    message("i.e., where trackloss proportion is greater than : ", round(prop_thresh_trial*100, 2), "%")
    exclude_trials_zs = paste(tl$Participant[tl$Trial_ZScore > trial_z_thresh], 
                              tl$Trial[tl$Trial_ZScore > trial_z_thresh], 
                              sep="_")
    message(paste("\t...removed ", length(exclude_trials_zs), " trials."))
  } else {
    exclude_trials_zs = c()
  }
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
  if (participant_z_thresh < Inf) {
    message("Will exclude participants whose trackloss-z-score is greater than : ", participant_z_thresh)
    prop_thresh_ppt = sd(tl$TracklossForParticipant)*participant_z_thresh + mean(tl$TracklossForParticipant)
    message("i.e., whose trackloss is greater than : ", round(prop_thresh_ppt*100, 2), "%")
    exclude_ppts_z = unique(tl$Participant[tl$Part_ZScore > participant_z_thresh])
    message(paste("\t...removed ", length(exclude_ppts_z), " participants."))
  } else {
    exclude_ppts_z = c()
  }
  if (participant_prop_thresh < 1) {
    message("Will exclude participants whose trackloss proportion is greater than : ", participant_prop_thresh)
    exclude_ppts_prop = unique(tl$Participant[tl$TracklossForParticipant > participant_prop_thresh])
    message(paste("\t...removed ", length(exclude_ppts_prop), " participants."))
  } else {
    exclude_ppts_prop = c()
  }
  
  exclude_trials = c(exclude_trials_zs,
                     exclude_trials_props,
                     unique( data$TrialID[part_vec %in% exclude_ppts_z] ),
                     unique( data$TrialID[part_vec %in% exclude_ppts_prop] ))
  
  # Remove:
  data_clean = data %>%
    filter(! .TrialID %in% exclude_trials)

  data_clean$.TrialID = NULL
  
  data_clean

}

# convert_non_aoi_to_trackloss ()
#
# Should gaze outside of any AOI be considered trackloss? This function sets trackloss to TRUE for any 
# samples that were not inside an AOI
#
# @param dataframe data
# @param list data_options
#
# @return dataframe 

convert_non_aoi_to_trackloss = function(data, data_options) {
  replace_na_arg = lapply(data_options$aoi_columns,
                          FUN = function(aoi) make_dplyr_argument("ifelse(is.na(",aoi,"), 0, ", aoi,")" ))
  
  names(replace_na_arg) = paste0(".", data_options$aoi_columns)
  
  out = data %>%
    mutate_(.dots = replace_na_arg) %>%
    mutate_(.dots = list(.Trackloss = make_dplyr_argument(data_options$trackloss_column))) %>%
    mutate_(.dots = list(.AOISum = make_dplyr_argument(paste(paste0('.',data_options$aoi_columns), collapse = "+"))) ) %>%
    mutate(.Trackloss = ifelse(.AOISum == 0, TRUE, .Trackloss))
  
  out[[data_options$trackloss_column]] <- out$.Trackloss
    
  for (aoi in paste0(".", data_options$aoi_columns)) {
    out[[aoi]] = NULL
  }
  
  out[[".AOISum"]] = NULL
  out[[".Trackloss"]] = NULL
  
  out
}

# keep_trackloss ()
#
# Converts data so that, in all subsequent proportion-looking calculations, 
# proportion_looking_at_aoi = time_looking_at_aoi / time_possible.
# Trackloss is not considered missing data, but instead is counted as time
# *not* spent looking at each AOI.
#
# @param dataframe data
# @param list data_options
#
# @return dataframe 
keep_trackloss = function(data, data_options) {
  
  # Set Looking-at-AOI to FALSE for any samples where there is Trackloss:
  replace_trackloss_arg = 
    lapply(data_options$aoi_columns, FUN = function(aoi) make_dplyr_argument("ifelse(",data_options$trackloss_column,"==1, 0, ", aoi,")" ))
  names(replace_trackloss_arg) = data_options$aoi_columns
  out= data %>%
    mutate_(.dots= replace_trackloss_arg)
  
  # If there is still missing data for AOIs, warn that it's going to be ignored:
  for (aoi in data_options$aoi_columns) {
    if (any(is.na(data[[aoi]]))) {
      warning("NAs found for non-trackloss samples, in '", aoi, 
              "'' column. These samples will be interpreted as being outside of the ", aoi, " AOI.")
    }
  }

  # Replace any Lingering Missing AOI Data:
  replace_na_arg = 
    lapply(data_options$aoi_columns, FUN = function(aoi) make_dplyr_argument("ifelse(is.na(",aoi,"), 0, ", aoi,")" ))
  names(replace_na_arg) = data_options$aoi_columns
  out= out %>%
    mutate_(.dots= replace_na_arg)
  
  out
}

# remove_trackloss ()
#
# Converts data so that, in all subsequent proportion-looking calculations, 
# proportion_looking_at_aoi = time_looking_at_aoi / time_looking
# Trackloss *is* considered missing data, and time spent not looking at a given AOI
# must have been spent looking at another AOI
#
# @param dataframe data
# @param list data_options
# @param logical delete_rows (default: TRUE) Should rows with trackloss be deleted, throwing out all of the other useful information those rows might hold?
#
# @return dataframe 
remove_trackloss = function(data, data_options, delete_rows = TRUE) {
  if (delete_rows) {
    # Remove all rows with Trackloss:
    out = data %>%
           mutate_(.dots = list(
             TracklossBoolean = make_dplyr_argument('ifelse(is.na(', data_options$trackloss_column, '), 0, ', data_options$trackloss_column, ')')
             )) %>%
           filter(TracklossBoolean == 0)
    
  } else {
    # Set Looking-at-AOI to NA for any samples where there is Trackloss:
    filter_trackloss_arg = 
      lapply(data_options$aoi_columns, FUN = function(aoi) make_dplyr_argument("ifelse(!is.na(",data_options$trackloss_column,"), NA, ", aoi,")" ))
    names(filter_trackloss_arg) = data_options$aoi_columns
    out = data %>%
          mutate_(.dots= filter_trackloss_arg)
  }
  
  out
}

# Shaping ------------------------------------------------------------------------------------------

# window_shape()
#
# Collapse time across our entire window and return a dataframe ready for LMERing
#
# @param dataframe data
# @param list data_options
# @param string aoi
# @param character.vector condition_columns
# @param numeric.vector window
#
# @return dataframe
window_shape <- function(data, 
                            data_options, 
                            aoi, 
                            condition_columns = NULL,
                            summarize_by = 'crossed'
) {
  require('dplyr', quietly=TRUE)
  
  # Prelims:
  data = ungroup(data)
  dopts = data_options
  
  # For Multiple DVs:
  if (length(aoi) > 1) {
    list_of_dfs = lapply(X = aoi, FUN = function(this_dv) {
      message("Analyzing ", this_dv, "...")
      window_shape(data, data_options, aoi = this_dv, condition_columns)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('window_shape', class(out))
    return( out )
  }
  
  # How to Group? By Sub? Item? Both?
  group_by_arg = switch(match.arg(summarize_by, c('crossed', 'subjects', 'participants', 'items')),
                        crossed      = as.list( c(dopts$participant_column, dopts$item_columns, condition_columns) ),
                        subjects     = as.list( c(dopts$participant_column, condition_columns) ),
                        participants = as.list( c(dopts$participant_column, condition_columns) ),
                        items        = as.list( c(dopts$item_columns, condition_columns) )
  )
  
  # Summarise:
  summarized = data %>% 
    group_by_(.dots = group_by_arg ) %>%
    summarise_( .dots = list(SamplesInAOI = make_dplyr_argument( "sum(", aoi, ", na.rm= TRUE)" ),
                             SamplesTotal = make_dplyr_argument( "sum(!is.na(", aoi, "))" ) # ignore all NAs 
    ) ) %>%
    mutate(AOI = aoi,
           elog = log( (SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5) ) ,
           weights = 1 / ( ( 1 / (SamplesInAOI + .5) ) / ( 1 / (SamplesTotal - SamplesInAOI +.5) ) ),
           Prop = SamplesInAOI / SamplesTotal,
           ArcSin = asin( sqrt( Prop ) )
    ) %>%
    ungroup()
  
  class(summarized) = c('window_shape', class(summarized))
  return(summarized)
  
}



#' time_shape()
#' 
#' Creates time-bins and summarizes proportion-looking within each time-bin, by-participants,
#' by-items, or crossed. Returns the summarized dataframe, ready for timecourse analyses.
#' 
#' @param dataframe data
#' @param list data_options
#' @param integer time_bin_size The time (ms) to fit into each bin
#' @param string aoi Which AOI(s) do you want to analyze?
#' @param character.vector condition_columns 
#' @param character summarize_by Should the data by summarized by participant, by item, or crossed (both)?
#' 
#' @return dataframe summarized

time_shape <- function (data, 
                           data_options, 
                           time_bin_size = 250, 
                           aoi = data_options$aoi_columns, 
                           condition_columns = NULL,
                           summarize_by = 'crossed') {
  
  
  require('dplyr', quietly=TRUE)
  
  # For Multiple aois:
  if (length(aoi) > 1) {
    list_of_dfs = lapply(X = aoi, FUN = function(this_aoi) {
      message("Creating Summary for ", this_aoi, "...")
      time_shape(data, data_options, time_bin_size, this_aoi, condition_columns, summarize_by)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('time_shape', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data)
  dopts = data_options
  
  # Check that Condition Column has a healthy number of levels (i.e., won't crash R)
  num_unique_conditions_in_cell = lapply(X = condition_columns, 
                                         FUN = function(condition_col) make_dplyr_argument( "length(unique(",condition_col,"))" ))
  names(num_unique_conditions_in_cell) = condition_columns
  df_cond_check = data %>%
    group_by_(.dots = as.list( c(dopts$participant_column, dopts$trial_column) )) %>%
    summarise_(.dots = num_unique_conditions_in_cell )
  for (condition_col in condition_columns) {
    if (any(df_cond_check[[condition_col]] > 1)) {
      stop("Condition columns should not vary within a trial")
    }
  }
  
  # How to Group? By Sub? Item? Both?
  group_by_arg = switch(match.arg(summarize_by, c('crossed', 'subjects', 'participants', 'items')),
                        crossed      = as.list( c(dopts$participant_column, dopts$item_columns, condition_columns, "TimeBin") ),
                        subjects     = as.list( c(dopts$participant_column, condition_columns, "TimeBin") ),
                        participants = as.list( c(dopts$participant_column, condition_columns, "TimeBin") ),
                        items        = as.list( c(dopts$item_columns, condition_columns, "TimeBin") )
  )
  
  
  # Make summarized dataframe:
  summarized = data %>%
    # Make Time Bins:
    mutate_(
      .dots = list(TimeBin = make_dplyr_argument("floor(", dopts$time_column, "/", time_bin_size, ")" ) )
    ) %>%
    
    # Group by Sub/Item, Summarize Samples in AOI:
    group_by_(.dots =  group_by_arg) %>%
    summarise_(.dots =  list(SamplesInAOI = make_dplyr_argument( "sum(", aoi, ", na.rm=TRUE)"),
                             SamplesTotal = make_dplyr_argument( "sum(!is.na(", aoi, "))")
    )) %>%
    
    # Add Time -- a time value that perfectly corresponds to TimeBin
    mutate_(.dots = list(Time = make_dplyr_argument('TimeBin * ', time_bin_size))) %>%
    
    # Compute Proportion, Empirical Logit, etc.:
    mutate(AOI = aoi,
           elog = log( (SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5) ),
           weights = 1 / ( ( 1 / (SamplesInAOI + .5) ) / ( 1 / (SamplesTotal - SamplesInAOI +.5) ) ),
           Prop = SamplesInAOI / SamplesTotal,
           ArcSin = asin( sqrt( Prop ) )
    ) %>%
    ungroup()
  
  # add orthogonal polynomials for Growth Curve Analyses
  time_bin_column <- summarized$TimeBin
  max_degree = min( length(unique(time_bin_column))-1 , 7 )
  if (max_degree < 7) warning("Fewer time bins than polynomial degrees-- consider decreasing size of time bin.")
  orthogonal_polynomials <- poly(sort(as.vector(unique(time_bin_column))), max_degree)
  time_codes <- data.frame(
    sort(as.vector(unique(time_bin_column))),
    orthogonal_polynomials[, c(1:max_degree)]
  )
  colnames(time_codes) <- c('TimeBin',paste0("ot", 1:max_degree))
  
  summarized <- merge(summarized, time_codes, by='TimeBin')
  
  class(summarized) = c('time_shape', class(summarized))
  
  return(summarized)
  
}

# cluster_analysis = function(data, data_options, condition_column, method, alpha, ...) {
#   UseMethod("cluster_analysis")
# }
# 
# #' cluster_analysis.data.frame()
# #' 
# #' @param dataframe data The output of the 'time_shape' function
# #' @param list data_options            
# #' ...
# #' @return dataframe 
# 
# cluster_analysis.data.frame = function(data, data_options, condition_column, method, alpha = .10) {
#   #[write me]
# }
# 
# cluster_analysis_helper = function(data, data_options, formula, stat_func, ...) {
#   
# }
# 
# #' cluster_analysis.time_shape()
# #' 
# #' Takes data that has been summarized into time-bins, and finds adjacent time bins that
# #' pass some threshold of significance, and assigns these adjacent groups into clusters
# #' for further examination.
# #' 
# #' @param dataframe.time_shape data The output of the 'time_shape' function
# #' @param list data_options            
# #' ...
# #' @return dataframe 
# 
# cluster_analysis.time_shape = function(data, data_options, condition_column, paired=FALSE, alpha = .10) {
#   
#   ## Helper:
#   label_clusters = function(vec) {
#     vec[is.na(vec)] = 0
#     out = c(cumsum(diff(vec)==1))
#     out[!vec] = NA
#     out
#   }
#   
#   ## Test Bins:
#   time_bin_summary = analyze_time_bins(data, data_options, condition_column, paired=paired, alpha = alpha)
#   
#   ## Label Adjacent Clusters:
#   time_bin_summary$Sig = time_bin_summary$Statistic > time_bin_summary$CritStatistic
#   time_bin_summary %>%
#     group_by(AOI) %>%
#     mutate(Cluster = label_clusters(Sig))
#   
#   cat("")
# }

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
#' @return dataframe 

onset_shape = function(data, data_options, onset_time, fixation_window_length = NULL, target_aoi, distractor_aoi = NULL) {
  require(zoo, quietly=TRUE)
  
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
  
  ## Translate TimeWindow units from time to number of rows (for rolling mean):
  df_time_per_row = data %>%
    group_by_(.dots = list(data_options$participant_column, data_options$trial_column) ) %>%
    summarise_(.dots = list(TimePerRow = make_dplyr_argument("mean(diff(",data_options$time_column,"))")))
  time_per_row = round(mean(df_time_per_row[["TimePerRow"]]))
  fixation_window_length_rows = fixation_window_length / time_per_row
  
  # Determine First AOI, Assign Switch Value for each timepoint:
  out = data %>%
    group_by_(.dots = list(data_options$participant_column, data_options$trial_column) ) %>%
    mutate_(.dots = list(.Target     = make_dplyr_argument("na_replace_rollmean(", target_aoi, ")"),
                         .Distractor = make_dplyr_argument("na_replace_rollmean(", distractor_aoi, ")"),
                         .Time       = make_dplyr_argument(data_options$time_column)
    ) ) %>%
    mutate(.ClosestTime = ifelse(length(which.min(abs(.Time - onset_time)))==1, .Time[which.min(abs(.Time - onset_time))], NA),
           FirstAOI     = ifelse(.Target[.Time==.ClosestTime] > .Distractor[.Time==.ClosestTime], target_aoi, distractor_aoi)) %>%
    ungroup() %>%
    mutate(FirstAOI     = ifelse(abs(.ClosestTime-onset_time) > fixation_window_length, NA, FirstAOI),
           WhichAOI     = ifelse(.Target > .Distractor, target_aoi, distractor_aoi),
           SwitchAOI    = FirstAOI != WhichAOI) %>%
    select(-.Target, -.Distractor, -.Time, -.ClosestTime) %>%
    ungroup()
  
  # Assign class information:
  class(out) = c('onset_shape', class(out))
  attr(out, 'onset_contingent') = list(distractor_aoi = distractor_aoi, 
                                       target_aoi = target_aoi, 
                                       onset_time = onset_time,
                                       fixation_window_length = fixation_window_length)
  
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

# Analyzing ------------------------------------------------------------------------------------------

#' analyze_time_bins()
#' 
#' Runs a test on each time-bin of a time-analysis. Defaults to a t-test, but supports wilcox, lm, and lmer as well.
#' 
#' @param dataframe.time_shape data The output of the 'time_shape' function
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
  
  require('dplyr', quietly=TRUE)
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
      crit_pos =  1.96
      crit_neg = -1.96
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
# Plot the confidence intervals that result from the 'analyze_time_bins' function
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
    ylab("T-Statistic") +
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
  
  # Brock: set smoothing_window_size based on fixation_window_length in attr's
  # TODO: any problem with this?
  smoothing_window_size <- onset_attr$fixation_window_length
  
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


# Helpers -----------------------------------------------------------------------------------------------

# make_dplyr_argument()
#
# Takes strings, and concatenates them into a formula, which will be properly passed as an
# NSE argument into a dplyr verb
#
# @param dots ... An indefinite amount of strings
#
# @return formula A formula that will be evaluated in the parent environment


make_dplyr_argument = function(...) {
  parts = list(...)
  arg_string = paste0("~", paste0(parts, collapse = " ") )
  return( as.formula(arg_string, env = parent.frame()) )
}

#' make_dplyr_expression()
#' 
#' Create an expression for a dplyr verb. Instead of writing out the bare expression, you write it as a string
#' with fill-in-the-blank style method for filling in programmatically-defined argument names.
#' 
#' The upshot is this is a clean-syntax way of using dplyr programmatically. E.g.,
#'
#' > mutate_(.dots = list(MeanRT = make_dplyr_expression("mean({reaction_time_column}, na.rm=TRUE)", reaction_time_column = "RT") ) )
#' 
#' @param character  the_expression  A string containing an expression, with placeholders wrapped in curly braces
#' @param ...        ...             More args, where names are to-be-replaced placeholders in the expression
#'                                   (not including curly braces), and values are the names of vars to be inserted
#' 
#' @return formula A formula that will be evaluated in the parent environment

make_dplyr_expression = function(the_expression, ...) {
  the_arguments = list(...)
  the_expression_new = the_expression
  for (i in seq_along(the_arguments)) {
    this_pattern = paste0("{", names(the_arguments)[i], "}")
    this_replacement = the_arguments[[i]]
    the_expression_new = gsub(pattern = this_pattern, replacement = this_replacement, x = the_expression_new, fixed = TRUE)
  }
  the_expression_new = paste("~", the_expression_new)
  return( as.formula(the_expression_new, env = parent.frame()) )
}


# center_predictors()
#
# Center predictors in preparation for statistical analyses
#
# @param dataframe data
# @param character.vector predictors
#
# @return dataframe data with modified columns appended with "C"

center_predictors = function(data, predictors) {
  require('dplyr', quietly=TRUE)
  
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

mutate_.time_shape = mutate_.window_shape = mutate_.bin_analysis = mutate_.onset_shape = function(data, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bin_analysis')
  temp_remove = class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr = attr(data, "onset_contingent") # also attributes
  
  out = mutate_(data, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  if ("onset_shape" %in% temp_remove) attr(out, "onset_contingent") = temp_attr
  
  return(out)
}

filter_.time_shape = filter_.window_shape = filter_.bin_analysis =  filter_.onset_shape = function(data, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bin_analysis')
  temp_remove = class(data)[ class(data) %in% potential_classes]
  class(data) = class(data)[!class(data) %in% potential_classes]
  temp_attr = attr(data, "onset_contingent") # also attributes
  
  out = filter_(data, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out) )
  if ("onset_shape" %in% temp_remove) attr(out, "onset_contingent") = temp_attr
  
  return(out)
}

left_join.time_shape = left_join.window_shape = left_join.bin_analysis = left_join.onset_shape = function(x, y, by = NULL, copy = FALSE, ...) {
  
  # remove class names (avoid infinite recursion):
  potential_classes = c('time_shape', 'window_shape', 'onset_shape', 'bin_analysis')
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

