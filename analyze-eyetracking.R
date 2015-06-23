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
# Required columns in dataset (names are configurable):
#   * ParticipantName (participant subject code)
#   * SceneType (active AOI)
#   * A column for each active AOI, set to 1 or 0
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
# @param integer sample_rate Number of samples (per second) recorded by eyetracker
# @param string participant_column Set to the subject code of the participant
# @param string active_aoi_column Set to the name of the AOI being gazed at for this sample
# @param string trackloss_column Set to 1 or 0 depending on whether the sample is lost to trackloss
# @param string time_column The factor to use for judgments of time (ms)
# @param string sample_column The incrementing factor that numbers gaze samples (0,1,2,3,4,...)
# @param string trial_column The unique name of the current trial
# @param string default_dv The default column for your dependent variable, used if unspecified in *_analysis() methods
#
# @return list of configuration options

set_data_options <- function(
  sample_rate = 60,
  participant_column = 'ParticipantName',
  active_aoi_column = 'SceneType',
  trackloss_column = 'TrackLoss',
  time_column = 'TimeFromMovieOnset',
  sample_column = 'FramesFromMovieOnset',
  trial_column = 'Trial',
  item_columns = 'Trial',
  default_dv = 'ActionMatch',
  xpos_column = 'XPos',
  ypos_column = 'YPos',
  message_column = NULL
) {
  list(
    'sample_rate' = sample_rate,
    'participant_column' = participant_column,
    'active_aoi_column' = active_aoi_column,
    'trackloss_column' = trackloss_column,
    'time_column' = time_column,
    'sample_column' = sample_column,
    'trial_column' = trial_column,
    'item_columns' = item_columns,
    'default_dv' = default_dv,
    'xpos_column' = xpos_column,
    'ypos_column' = ypos_column,
    'message_column' = message_column
  )
}

# verify_dataset()
#
# Verify, and fix, the status of the dataset by assessing columns in your data_options. 
# Assigns data to "sample_data" class, so that plot and other functions will know what to do with it.
#
# @param dataframe data
# @param list data_options
# @param boolean silent
#
# @return dataframe data

verify_dataset <- function(data, data_options) {
  
  out = data
  class(out) = c("sample_data", class(out))
  
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
                            "sample_column"      = function(x) check_then_convert(x, is.numeric, as.numeric2, "Sample"),
                            "trial_column"       = function(x) check_then_convert(x, is.factor, as.factor, "Trial"),
                            "item_columns"       = function(x) check_then_convert(x, is.factor, as.factor, "Item")
  )
  for (col in names(col_type_converter) ) {
    for(i in seq_along(data_options[[col]])) {
      if (is.null(out[[ data_options[[col]][i] ]])) stop("Data are missing", col)
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
  require(dplyr)
  
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
  message("Subsetting by window...")
  out = data %>%
    filter_(.dots = list(make_dplyr_argument(dopts$time_column, "> .WindowStart"),
                         make_dplyr_argument(dopts$time_column, "< .WindowEnd")
            )
    )
  
  # Rezero
  if (rezero) {
    message("Rezeroing timestamp...")
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

describe_data = function(data, data_options, dv = data_options$default_dv, factors) {
  require(dplyr)
  
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
# @param numeric.vector time_window First number specifies start of timewindow, second specifies end of timewindow
#
# @return dataframe 

trackloss_analysis = function(data, data_options, time_window = NULL) {
  
  .zscore = function(x) (x-mean(x,na.rm=TRUE)) / sd(x,na.rm=TRUE)
  require('dplyr')
    
  # Prelims:
  dopts = data_options
  data[["Trackloss"]] = data[[dopts$trackloss_column]]
  if (is.null(time_window)) {
    time_window = range(data[dopts$time_column])
  }
  
  data %>%
    # Filter by Time-Window:
    filter_(.dots = list( make_dplyr_argument(dopts$time_column, ">=", time_window[1]) , 
                          make_dplyr_argument(dopts$time_column, "<=", time_window[2]) )
            ) %>%
    # Get Trackloss-by-Trial:
    group_by_(.dots = list(dopts$participant_column, dopts$trial_column)) %>%
    mutate(TracklossForTrial = mean(Trackloss, na.rm=TRUE)) %>%
    ungroup() %>%
    # Get Trackloss-by-Participant:
    group_by_(.dots = list(dopts$participant_column)) %>%
    mutate(TracklossForParticipant = mean(Trackloss, na.rm=TRUE)) %>%
    ungroup() %>%
    # Get Z-Scores:
    group_by_(.dots = list(dopts$participant_column, dopts$trial_column) )%>%
    summarise(TracklossForTrial = mean(TracklossForTrial, na.rm=TRUE),
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
#
# @return dataframe 

clean_by_trackloss = function(data, data_options, participant_z_thresh = Inf, trial_z_thresh = Inf) {
  
  data$TrialID = paste(data[[data_options$participant_col]], data[[data_options$trial_col]], sep = "_")
  
  # Trackloss Analysis:
  message("Performing Trackloss Analysis...")
  tl = trackloss_analysis(data, data_options)
  
  # Bad Trials:
  message("Will exclude trials whose trackloss-z-score is greater than : ", trial_z_thresh)
  exclude_trials = paste(tl$Participant[tl$Trial_ZScore > trial_z_thresh], 
                         tl$Trial[tl$Trial_ZScore > trial_z_thresh], 
                         sep="_")
  
  # Bad Participants
  message("Will exclude participants whose trackloss-z-score is greater than : ", trial_z_thresh)
  part_vec = data[[data_options$participant_col]]
  exclude_ppts = unique(tl$Participant[tl$Part_ZScore > participant_z_thresh])
  exclude_trials = c(exclude_trials, 
                     unique( data$TrialID[part_vec %in% exclude_ppts] ) )
  
  # Remove:
  data_clean = data %>%
    filter(! TrialID %in% exclude_trials) 

  data_clean
}

# generate_random_data()
#
# Generate a random set of looking time data
#
# @param list data_options
# @param integer seed (optional)
#
# @return dataframe data
generate_random_data <- function (data_options, seed = NA, num_participants = 20) {
  # if we have a seed, set it so that we can generate a consistent dataset
  # perhaps to follow along in a tutorial
  if (!is.na(seed)) {
    set.seed(seed)
  }
  
  # Experiment design
  #
  # Looking while listening, look to target
  # 
  # Will children look towards objects thematically associated with the heard word?
  #
  # Conditions:
  #     Related, one of the objects is related to the word
  #     NotRelated, no thematic matches
  
  # set participants
  participants <- c(1:num_participants)
  
  # Rows
  #
  # We will have 8 seconds of total looking in each of our 6 trials for 20 participants
  #   baseline: 4 seconds
  #   response: 4 seconds
  
  num_rows <- data_options$sample_rate*8*6*max(participants)
  
  # Columns
  #
  # ParticipantName
  # Age
  # Condition
  # TrialNum
  # Trial
  # Window
  # Time
  # Sample
  # Trackloss
  # ActiveAOI (Target, Distractor)
  # Target
  # Distractor
  
  data <- data.frame(matrix(nrow = num_rows, ncol = 12))
  colnames(data) <- c(data_options$participant_column, 
                      'Age', 
                      'Condition', 
                      'TrialNum', 
                      data_options$trial_column, 
                      'Window', 
                      data_options$time_column, 
                      data_options$sample_column, 
                      data_options$trackloss_column, 
                      data_options$active_aoi_column, 
                      'Target', 
                      'Distractor')
  
  # calculate the general slopes by condition
  
  # break the response period into 16 phases, and use a log function to
  # increase the likelihood of a look towards the Target
  log_target <- rep(log(seq(1,16), 100000),each=((data_options$sample_rate*4)/16))
  
  # add some error
  error <- rnorm((data_options$sample_rate*4),0,.15)
  
  # calculate looking to target (>.5 == a target look)
  y <- .5 + log_target + error
  
  # fix ceiling/floor hits
  y[which(y > 1.0)] <- 1.0
  y[which(y < 0)] <- 0
  
  Related <- y
  
  # and again...
  error <- rnorm((data_options$sample_rate*4),0,.07)
  y <- .55 + error
  y[which(y > 1.0)] <- 1.0
  y[which(y < 0)] <- 0
  
  NotRelated <- y
  
  for (x in participants) {
    # this participant's data will live in these rows
    row_range <- seq((((x - 1)*(data_options$sample_rate*8*6))+1),(x*data_options$sample_rate*8*6))
    
    participant_id <- paste('SUBJ_',x,sep="")
    condition <- ifelse(x <= floor(length(participants) / 2), 'Knower','NonKnower')
    data[row_range, data_options$participant_column] <- participant_id
    data[row_range, 'Age'] <- round(rnorm(1,24,.25), 2)
    data[row_range, 'Condition'] <- condition
    data[row_range, 'TrialNum'] <- rep(c(1:6),each=(data_options$sample_rate*8))
    data[row_range, data_options$trial_column] <- rep(c('Chimpanzee','Bowl','Speaker','Woman','Pen','Basketball'),each=(data_options$sample_rate*8))
    data[row_range, data_options$sample_column] <- rep(c(1:(data_options$sample_rate*8)),times=6)
    data[row_range, data_options$time_column] <- (data[row_range, data_options$sample_column]-1)*(1000/data_options$sample_rate)
    
    # generate trackloss probability for this participant
    trackloss_probability <- runif(1,.1,0.35)
    
    data[row_range, data_options$trackloss_column] <- rbinom((data_options$sample_rate*8*6),1,trackloss_probability)
    
    # for baseline, they have a 50% chance of looking to the target or distractor
    data[row_range, data_options$active_aoi_column] <- 0
    
    # AOI assignment is by trial
    for (y in 1:6) {
      # for baseline, let's just give everyone a 50/50 chance
      baseline_range <- 
        which(data[, data_options$participant_column] == participant_id & data[, data_options$sample_column] <= (data_options$sample_rate*4) & data[, 'TrialNum'] == y)
      
      data[baseline_range, 'Window'] <- 'Baseline'
      data[baseline_range, 'Target'] <- ifelse(rbinom((data_options$sample_rate*4),1,0.5) == 1,1,0)
      
      # for response, we can set the target/distractor looks using the conditions
      # sloped probability (defined above)
      response_range <- 
        which(data[, data_options$participant_column] == participant_id & data[, data_options$sample_column] > (data_options$sample_rate*4) & data[, 'TrialNum'] == y)
      
      data[response_range, 'Window'] <- 'Response'
      
      if (condition == 'Knower') {
        data[response_range, 'Target'] <- Related
      }
      else {
        data[response_range, 'Target'] <- NotRelated
      }
      
      data[response_range, 'Target'] <- sapply(data[response_range, 'Target'], function (x) {
        rbinom(1,1,as.numeric(x))
      });
    }
    
    # set AOI columns from active_aoi_column
    data[which(data[, 'Target'] == 1), data_options$active_aoi_column] <- 'Target'
    data[which(data[, 'Target'] == 1), 'Distractor'] <- 0
    data[which(data[, 'Target'] == 0), data_options$active_aoi_column] <- 'Distractor'
    data[which(data[, 'Target'] == 0), 'Distractor'] <- 1
    
    # no AOI for trackloss
    data[which(data[, data_options$trackloss_column] == 1), data_options$active_aoi_column] <- NA
    data[which(data[, data_options$trackloss_column] == 1), 'Target'] <- NA
    data[which(data[, data_options$trackloss_column] == 1), 'Distractor'] <- NA
  }
  
  # verify and format dataset
  data <- verify_dataset(data, data_options, silent = TRUE)
  
  data$Condition <- factor(data$Condition)
  data$Window <- factor(data$Window)
  data[, data_options$active_aoi_column] <- factor(data[, data_options$active_aoi_column])
  
  data
}


# Analyzing ------------------------------------------------------------------------------------------

# window_analysis()
#
# Collapse time across our entire window and return a dataframe ready for LMERing
#
# @param dataframe data
# @param list data_options
# @param string dv
# @param character.vector condition_columns
# @param numeric.vector window
#
# @return dataframe
window_analysis <- function(data, 
                            data_options, 
                            dv = data_options$default_dv, 
                            condition_columns = NULL
                            ) {
  
  require('dplyr')
  
  # Prelims:
  data = ungroup(data)
  dopts = data_options
  
  # For Multiple DVs:
  if (length(dv) > 1) {
    list_of_dfs = lapply(X = dv, FUN = function(this_dv) {
      message("Analyzing", this_dv, "...")
      window_analysis(data, data_options, dv = this_dv, condition_columns)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('window_analysis', class(out))
    return( out )
  }
  
  
  # Summarise:
  summarized = data %>% 
    group_by_(.dots = as.list(c(dopts$participant_column, dopts$trial_column, dopts$item_columns, condition_columns)) ) %>%
    summarise_( .dots = list(SamplesInAOI = make_dplyr_argument( "sum(", dv, ", na.rm= TRUE)" ),
                             SamplesTotal = make_dplyr_argument( "sum(!is.na(", dv, "))" ) # ignore all NAs 
    ) ) %>%
    mutate(AOI = dv,
           elog = log( (SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5) ) ,
           weights = 1 / ( ( 1 / (SamplesInAOI + .5) ) / ( 1 / (SamplesTotal - SamplesInAOI +.5) ) ),
           Prop = SamplesInAOI / SamplesTotal,
           ArcSin = asin( sqrt( Prop ) )
    )
  
  class(summarized) = c('window_analysis', class(summarized))
  return(summarized)
  
}



# time_analysis()
#
# Creates time-bins and summarizes proportion-looking within each time-bin, by-participants,
# by-items, or crossed. Returns the summarized dataframe, ready for timecourse analyses.
#
# @param dataframe data
# @param list data_options
# @param integer time_bin_size The time (ms) to fit into each bin
# @param string dv The dependent variable column
# @param character.vector condition_columns 
# @param character summarize_by Should the data by summarized by participant, by item, or crossed (both)?
#
# @return dataframe summarized
time_analysis <- function (data, 
                           data_options, 
                           time_bin_size = 250, 
                           dv = data_options$default_dv, 
                           condition_columns = NULL,
                           summarize_by = 'crossed') {
  
  
  require('dplyr')
  
  # For Multiple DVs:
  if (length(dv) > 1) {
    list_of_dfs = lapply(X = dv, FUN = function(this_dv) {
      message("Creating Summary for", this_dv, "...")
      time_analysis(data, data_options, time_bin_size, this_dv, condition_columns, summarize_by)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('time_analysis', class(out))
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
      .dots = list(TimeBin = make_dplyr_argument("ceiling(", dopts$time_column, "/", time_bin_size, ")" ) )
    ) %>%
    
    # Group by Sub/Item, Summarize Samples in AOI:
    group_by_(.dots =  group_by_arg) %>%
    summarise_(.dots =  list(SamplesInAOI = make_dplyr_argument( "sum(", dv, ", na.rm=TRUE)"),
                             SamplesTotal = make_dplyr_argument( "sum(!is.na(", dv, "))"),
                             StartTime    = make_dplyr_argument( dopts$time_column, "[1]")
    )) %>%
    
    # Compute Proportion, Empirical Logit, etc.:
    mutate(AOI = dv,
           elog = log( (SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5) ),
           weights = 1 / ( ( 1 / (SamplesInAOI + .5) ) / ( 1 / (SamplesTotal - SamplesInAOI +.5) ) ),
           Prop = SamplesInAOI / SamplesTotal,
           ArcSin = asin( sqrt( Prop ) )
    ) %>%
    ungroup()
  
  
  class(summarized) = c('time_analysis', class(summarized))
  
  return(summarized)
  
}

# analyze_time_bins()
#
# @param dataframe.time_analysis data The output of the 'time_analysis' function
#...
# @return dataframe 
analyze_time_bins <- function(data, 
                              data_options, 
                              condition_column,
                              within = FALSE,
                              return_model = FALSE,
                              dv_type = 'elog')
{
  
  require('dplyr')
  require('broom')
  
  ## Main: ===
  
  # Must be a time_analysis:
  if (!'time_analysis' %in% class(data)) stop('This function can only be run on the output of the "time_analysis" function.')
  
  # Only support one-way anova:
  if (length(condition_column)>1) stop('This function only supports one-way ANOVA (single condition column).')

  # For Multiple DVs:
  dvs = unique(data[['AOI']])
  if ( length(dvs) > 1 ) {
    list_of_dfs = lapply(X = dvs, FUN = function(this_dv) {
      message("Analyzing", this_dv, "...")
      this_df = filter(data, AOI == this_dv)
      class(this_df) = class(data)
      analyze_time_bins(data = this_df, data_options, condition_column, within, return_model)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('seq_bin', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data) # shouldn't be necessary, but just in case
  dopts = data_options
  
  # Collapse by Participant:
  dv_type = match.arg(dv_type, choices = c("ArcSin", "elog", "Prop"))
  summarise_arg = list(make_dplyr_argument("mean(",dv_type,",na.rm=TRUE)"))
  names(summarise_arg) = c(dv_type)
  by_part = data %>% 
    group_by_(.dots = as.list(c(dopts$participant_column, condition_column, "AOI", "TimeBin") ) ) %>%
    summarise_(.dots = summarise_arg)
  
  # Do an ANOVA for each TimeBin:
  the_formula = paste(dv_type, "~", condition_column)
  if (within) {
    message("Performing within-subject ANOVAs for time-bins.")
    the_formula = paste(the_formula, "+", "Error(", dopts$participant_column, "/", condition_column, ")")
  } else {
    message("Performing between-subject ANOVAs for time-bins.")
  }
  aov_models = lapply(X = unique(by_part$TimeBin), FUN = function(tb) {
    aov(formula = as.formula(the_formula), data = filter(by_part, TimeBin==tb), x = TRUE, y = TRUE)
  })
  
  # Extract F and Critical F:
  model_params = lapply(aov_models, FUN = function(x) tidy(x) )   
  f_vals = sapply(X = model_params, FUN = function(x) x[['statistic']][1+within])
  numer  = sapply(X = model_params, FUN = function(x) x[['df']][1+within])
  denom  = sapply(X = model_params, FUN = function(x) x[['df']][2+within])
  crit_f = qf(p = .975, df1 = numer, df2= denom)

  # Summarize:
  out = data.frame(stringsAsFactors = FALSE,
                   F= f_vals, 
                   CritF = crit_f, 
                   TimeBin = unique(by_part$TimeBin),
                   AOI = unique(by_part$AOI))
  if (return_model) out$Model = aov_models
  
  class(out) = c('seq_bin', class(out))
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


# plot.window_analysis()
#
# Plots the result of a window analysis
#
# @param dataframe data
# @param list data_options
# @param character condition_columns Maximum 2
#
# @return list A ggplot list object  
plot.window_analysis <- function(data, data_options, x_axis_column, group_column = NULL) {
  
  dopts = data_options
  
  # Organize Vars:
  if ( !is.null(group_column) ) {
    if (is.numeric(data[[group_column]])) {
      message("The variable '", group_column, "' is continous, will perform median split for visualization.")
      split_col = paste0(group_column, "_Split")
      data[[split_col]] = ifelse(data[[group_column]] > median(data[[group_column]]), 'High', 'Low')
      group_column = split_col
    }
  }
  color_var = group_column
  group_var = ifelse( is.null(group_column), 1, group_column)
  
  # Summarize by Participants:
  summarized = data %>%
    group_by_(.dots = c(dopts$participant_column, x_axis_column, group_column, "AOI")) %>%
    summarise(Prop = mean(Prop, na.rm= TRUE) )
  
  # Plot:
  if ( is.numeric(summarized[[x_axis_column]]) ) {
    ggplot(summarized, aes_string(x = x_axis_column, y = "Prop", group= group_var, color= color_var)) +
      stat_smooth(method="lm") +
      facet_wrap( ~ AOI) +
      ylab("Proportion Looking")
  } else {
    ggplot(summarized, aes_string(x = x_axis_column, y = "Prop", group= group_var, color= color_var)) +
      stat_summary(fun.y = mean, geom='line') +
      stat_summary(fun.dat = mean_cl_boot) +
      facet_wrap( ~ AOI) +
      ylab("Proportion Looking")
  }

}

# plot.time_analysis()
#
# Plot the timecourse of looking across groups. Median split if factor is continous
#
# @param dataframe data
# @param list data_options
# @param character condition_column
#
# @return list A ggplot list object  
plot.time_analysis <- function(data, data_options, condition_column) {
  
  require('ggplot2')
  
  # Prelims:
  data = ungroup(data)
  dopts = data_options
  
  # Check condition factor
  if ( length(condition_column) > 1 ) {
    stop('Can only plot time-analysis for one factor at a time.')
  }
  if ( is.numeric(data[[condition_column]]) ) {
    message("Condition factor is numeric, performing median split...")
    median_split_arg = list(GroupFactor = 
                              make_dplyr_argument("ifelse(", condition_column, ">median(", condition_column, ", na.rm=TRUE), 'High', 'Low')" )
    )
    out = data %>%
      mutate_(.dots = median_split_arg) %>%
      ggplot(aes(x = StartTime, y= Prop, group=GroupFactor, color=GroupFactor)) +
      stat_smooth() + 
      facet_wrap( ~ AOI) +
      guides(color= guide_legend(title= condition_column)) +
      xlab('Time (ms) in Trial')
    return(out)
  } else {
    out = ggplot(data, aes_string(x = "StartTime", y= "Prop", group= condition_column, color= condition_column)) +
      stat_smooth() + 
      facet_wrap( ~ AOI) +
      xlab('Time (ms) in Trial')
    return(out)
  }
  
}


# plot.seq_bin()
#
# Plot the confidence intervals that result from the 'analyze_time_bins' function
#
# @param dataframe data
#
# @return list A ggplot list object  
plot.seq_bin <- function(data) {
  
  require('ggplot2')
    
  ggplot(data = data) +
    geom_line(mapping = aes(x = TimeBin, y= F)) +
    geom_line(mapping = aes(x = TimeBin, y= CritF), linetype="dashed") +
    ylab("F-Statistic") +
    xlab("TimeBin") +
    facet_wrap( ~ AOI)
  
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


# center_predictors()
#
# Center predictors in preparation for statistical analyses
#
# @param dataframe data
# @param character.vector predictors
#
# @return dataframe data with modified columns appended with "C"

center_predictors = function(data, predictors) {
  require('dplyr')
    
  mutate_argument = list()
  for (i in seq_along(predictors)) {
    name = paste0(predictors[i], "C")
    mutate_argument[[name]] = make_dplyr_argument("as.numeric(", predictors[i], ") - mean(as.numeric(",  predictors[i], "), na.rm=TRUE)" )
  }

  data %>% 
    ungroup() %>%
    mutate_(.dots = mutate_argument)
  
}





