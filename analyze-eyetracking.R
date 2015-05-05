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
# @param string participant_factor Set to the subject code of the participant
# @param string active_aoi_factor Set to the name of the AOI being gazed at for this sample
# @param string trackloss_factor Set to 1 or 0 depending on whether the sample is lost to trackloss
# @param string time_factor The factor to use for judgments of time (ms)
# @param string sample_factor The incrementing factor that numbers gaze samples (0,1,2,3,4,...)
# @param string trial_factor The unique name of the current trial
# @param string default_dv The default column for your dependent variable, used if unspecified in *_analysis() methods
# @param character.vector default_factors The default columns for your indepdent variables, used if unspecified in *_analysis() methods
#
# @return list of configuration options

set_data_options <- function(
  sample_rate = 60,
  participant_factor = 'ParticipantName',
  active_aoi_factor = 'SceneType',
  trackloss_factor = 'TrackLoss',
  time_factor = 'TimeFromMovieOnset',
  sample_factor = 'FramesFromMovieOnset',
  trial_factor = 'Trial',
  item_factors = 'Trial',
  default_dv = 'ActionMatch',
  default_factors = c('Condition'),
  xpos_factor = 'XPos',
  ypos_factor = 'YPos',
  message_factor = NULL
) {
  list(
    'sample_rate' = sample_rate,
    'participant_factor' = participant_factor,
    'active_aoi_factor' = active_aoi_factor,
    'trackloss_factor' = trackloss_factor,
    'time_factor' = time_factor,
    'sample_factor' = sample_factor,
    'trial_factor' = trial_factor,
    'item_factors' = item_factors,
    'default_dv' = default_dv,
    'default_factors' = default_factors,
    'xpos_factor' = xpos_factor,
    'ypos_factor' = ypos_factor,
    'message_factor' = message_factor
  )
}

# repair_timestamp_data
#
# The main purpose of this function is to ensure that there is a column specifying the time within
# the trial, which is zeroed out at the timepoint of stimulus presentation. Additionally, a column specifying
# "post-trial" (after trial/stim end message) samples is created. Finally, a sample-index column is created if
# one does not already exist.
#
# @param dataframe data
# @param list data_options
# @param string trial_start_msg A string in the message_factor column that specifies when the trial started
# @param string trial_stop_msg  A string in the message_factor column that specifies when the trial ended
# @param boolean prompt If true, this function will ask before removing bad trials
#
# @return list of configuration options

repair_timestamp_data = function(data, 
                               data_options,
                               trial_start_msg = NULL,
                               trial_stop_msg = NULL,
                               prompt = TRUE) {
  
  require("dplyr")

  df = ungroup(data)
  dopts = data_options

  # Make Row for unique TrialID
  cat("\n\nMaking unique TrialID column...")
  df[['TrialID']] = paste(df[[dopts$participant_factor]], df[[dopts$trial_factor]], sep = "_")
  
  # Remove any trials in which the start or stop message in missing:
  df = df %>%
    group_by(TrialID) %>%
    mutate_(.dots = list(NoStartMsg = make(nse_arg( "all(!grepl('", trial_start_msg, "',", dopts$message_factor, "))" )),
                         NoStopMsg  = make(nse_arg( "all(!grepl('", trial_stop_msg,  "',", dopts$message_factor, "))", 
                                                    cond = !is.null(trial_stop_msg)) )
    ) )
  no_start_msg_trials = unique( df$TrialID[which(df$NoStartMsg)] )
  no_stop_msg_trials = unique( df$TrialID[which(df$NoStopMsg)] )
  
  if ( length(no_start_msg_trials) + length(no_stop_msg_trials) > 0) {
    cat("\nFound", length(no_start_msg_trials), "trials without a start msg, and ", length(no_stop_msg_trials),
        "without a stop message. These trials will be removed.")
    if (prompt) {
      readline("Press enter to continue. ")
    }
  }
  
  df = df %>%
    filter(!TrialID %in% no_start_msg_trials,
           !TrialID %in% no_stop_msg_trials)
    
  # Add TimeInTrial Column:
  cat("\n\nAdding TimeInTrial column...")
  ts_command = make(nse_arg(dopts$time_factor, " - ", dopts$time_factor, "[grep('", trial_start_msg, "',", dopts$message_factor, ")][1]"))
  df = df %>%
    group_by(TrialID) %>%
    mutate_(.dots = list(TimeInTrial = ts_command) ) %>%
    select(-NoStartMsg, -NoStopMsg) %>%
    ungroup()
  cat("\nAdded. You may want to set data_options$time_factor to 'TimeInTrial'")
  
  # Mark Post-Trial TSs:
  df = df %>%
    mutate_(.dots = list(PostTrial = 
                           make(nse_arg("TimeInTrial > TimeInTrial[grep('", trial_stop_msg, "',", dopts$message_factor, ")][1]",  
                                   cond = !is.null(trial_stop_msg) ))
    ))
  
  # Add Sample Index (if needed):
  if ( is.null(dopts$sample_factor) | is.null(df[[dopts$sample_factor]]) ) {
    cat("\n\nNo sample index column found, adding...")
    df = df %>%
      group_by(TrialID) %>%
      mutate(SampleIndex = 1:n()) 
    cat("\nAdded. You may want to set data_options$sample_factor to 'SampleIndex'")
  }

  return(ungroup(df))
  
}

# verify_dataset()
#
# Verify the status of the dataset by assessing columns in your data_options. Fix if neccessary.
# Also assigns data to "sample_data" class, so that plot and other functions will know what to do with it.
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
      warning('Converting ' , colname ,' to proper type.')
      convertfunc(x)
    } else {
      x
    }
  }
  col_type_converter = list("participant_factor" = function(x) check_then_convert(x, is.factor, as.factor, "Participants"),
                            "time_factor"        = function(x) check_then_convert(x, is.numeric, as.numeric2, "Time"),
                            "sample_factor"      = function(x) check_then_convert(x, is.numeric, as.numeric2, "Sample"),
                            "trial_factor"       = function(x) check_then_convert(x, is.factor, as.factor, "Trial"),
                            "item_factors"       = function(x) check_then_convert(x, is.factor, as.factor, "Item")
  )
  for (col in names(col_type_converter) ) {
    for(i in seq_along(data_options[[col]])) {
      if (is.null(out[[ data_options[[col]][i] ]])) stop("Data is missing", col)
      out[[ data_options[[col]][i] ]] = col_type_converter[[col]]( out[[ data_options[[col]][i] ]] )
    }
  }
  
  return( out )
  
}

# describe_data ()
#
# Describe a DV column in the dataset by a (group of) factor(s)
#
# @param dataframe data
# @param string dv
# @param character.vector factors
#
# @return dataframe 

describe_data = function(data, data_options, dv = data_options$default_dv, factors = data_options$default_factors) {
  require(dplyr)
  
  to_summarise = c("~mean(dv, na.rm= TRUE)",
                   "~sd(dv, na.rm= TRUE)",
                   "~var(dv, na.rm= TRUE)",
                   "~min(dv, na.rm= TRUE)",
                   "~max(dv, na.rm= TRUE)"
  )
  to_summarise = as.list( gsub(pattern = "dv", replacement = dv, x = to_summarise) ) %>%
    lapply(as.formula)
  names(to_summarise) = c("Mean", "SD", "Var", "Min", "Max")
  
  data %>%
    group_by_(.dots = as.list(factors)) %>%
    summarise_(.dots = to_summarise)
}

# trackloss_analysis ()
#
# Get information on trackloss
#
# @param dataframe data
# @param list data_options
#
# @return dataframe 

trackloss_analysis = function(data, data_options) {
  
  require('outliers')
  require('dplyr')
  
  dopts = data_options
  
  data[["Trackloss"]] = data[[dopts$trackloss_factor]]
  
  data %>%
    group_by_(.dots = list(dopts$participant_factor, dopts$trial_factor)) %>%
    mutate(TracklossForTrial = mean(Trackloss, na.rm=TRUE)) %>%
    ungroup() %>%
    group_by_(.dots = list(dopts$participant_factor)) %>%
    mutate(TracklossForParticipant = mean(Trackloss, na.rm=TRUE)) %>%
    ungroup() %>%
    group_by_(.dots = list(dopts$participant_factor, dopts$trial_factor) )%>%
    summarise(TracklossForTrial = mean(TracklossForTrial, na.rm=TRUE),
              TracklossForParticipant = mean(TracklossForParticipant, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Part_ZScore  = scores(TracklossForParticipant, type = "z"),
           Trial_ZScore = scores(TracklossForTrial, type = "z"),
           Part_MAD     = scores(TracklossForParticipant, type = "mad"),
           Trial_MAD    = scores(TracklossForTrial, type = "mad") )
  
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
  colnames(data) <- c(data_options$participant_factor, 
                      'Age', 
                      'Condition', 
                      'TrialNum', 
                      data_options$trial_factor, 
                      'Window', 
                      data_options$time_factor, 
                      data_options$sample_factor, 
                      data_options$trackloss_factor, 
                      data_options$active_aoi_factor, 
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
    data[row_range, data_options$participant_factor] <- participant_id
    data[row_range, 'Age'] <- round(rnorm(1,24,.25), 2)
    data[row_range, 'Condition'] <- condition
    data[row_range, 'TrialNum'] <- rep(c(1:6),each=(data_options$sample_rate*8))
    data[row_range, data_options$trial_factor] <- rep(c('Chimpanzee','Bowl','Speaker','Woman','Pen','Basketball'),each=(data_options$sample_rate*8))
    data[row_range, data_options$sample_factor] <- rep(c(1:(data_options$sample_rate*8)),times=6)
    data[row_range, data_options$time_factor] <- (data[row_range, data_options$sample_factor]-1)*(1000/data_options$sample_rate)
    
    # generate trackloss probability for this participant
    trackloss_probability <- runif(1,.1,0.35)
    
    data[row_range, data_options$trackloss_factor] <- rbinom((data_options$sample_rate*8*6),1,trackloss_probability)
    
    # for baseline, they have a 50% chance of looking to the target or distractor
    data[row_range, data_options$active_aoi_factor] <- 0
    
    # AOI assignment is by trial
    for (y in 1:6) {
      # for baseline, let's just give everyone a 50/50 chance
      baseline_range <- 
        which(data[, data_options$participant_factor] == participant_id & data[, data_options$sample_factor] <= (data_options$sample_rate*4) & data[, 'TrialNum'] == y)
      
      data[baseline_range, 'Window'] <- 'Baseline'
      data[baseline_range, 'Target'] <- ifelse(rbinom((data_options$sample_rate*4),1,0.5) == 1,1,0)
      
      # for response, we can set the target/distractor looks using the conditions
      # sloped probability (defined above)
      response_range <- 
        which(data[, data_options$participant_factor] == participant_id & data[, data_options$sample_factor] > (data_options$sample_rate*4) & data[, 'TrialNum'] == y)
      
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
    
    # set AOI columns from active_aoi_factor
    data[which(data[, 'Target'] == 1), data_options$active_aoi_factor] <- 'Target'
    data[which(data[, 'Target'] == 1), 'Distractor'] <- 0
    data[which(data[, 'Target'] == 0), data_options$active_aoi_factor] <- 'Distractor'
    data[which(data[, 'Target'] == 0), 'Distractor'] <- 1
    
    # no AOI for trackloss
    data[which(data[, data_options$trackloss_factor] == 1), data_options$active_aoi_factor] <- NA
    data[which(data[, data_options$trackloss_factor] == 1), 'Target'] <- NA
    data[which(data[, data_options$trackloss_factor] == 1), 'Distractor'] <- NA
  }
  
  # verify and format dataset
  data <- verify_dataset(data, data_options, silent = TRUE)
  
  data$Condition <- factor(data$Condition)
  data$Window <- factor(data$Window)
  data[, data_options$active_aoi_factor] <- factor(data[, data_options$active_aoi_factor])
  
  data
}

# Visualizing ------------------------------------------------------------------------------------------


# plot.time_analysis()
#
# Plot the timecourse of looking across groups. Median split if factor is continous
#
# @param dataframe data
# @param list data_options
# @param character condition_factor
#
# @return list A ggplot list object  
plot.time_analysis <- function(data, data_options, condition_factor = data_options$default_factors[1]) {
  
  require('ggplot2')
  
  # Prelims:
  data = ungroup(data)
  dopts = data_options
  
  # Check condition factor
  if ( length(condition_factor) > 1 ) {
    stop('Can only plot time-analysis for one factor at a time.')
  }
  if ( is.numeric(data[[condition_factor]]) ) {
    cat("\nCondition factor is numeric, performing median split...")
    median_split_arg = list(GroupFactor = 
                              make( nse_arg("ifelse(", condition_factor, ">median(", condition_factor, ", na.rm=TRUE), 'High', 'Low')") )
    )
    out = data %>%
      mutate_(.dots = median_split_arg) %>%
      ggplot(aes(x = StartTime, y= PropLooking, group=GroupFactor, color=GroupFactor)) +
      stat_smooth() + 
      facet_wrap( ~ AOI) +
      guides(color= guide_legend(title= condition_factor))
    return(out)
  } else {
    out = ggplot(aes_string(x = "StartTime", y= "PropLooking", group= condition_factor, color= condition_factor)) +
      stat_smooth() + 
      facet_wrap( ~ AOI)
    return(out)
  }
  
}


# plot.seq_bin()
#
# Plot the confidence intervals that result from the 'analyze_time_bins' function
#
# @param dataframe data
# @param list data_options
#
# @return list A ggplot list object  
plot.seq_bin <- function(data, data_options) {
  
  require('ggplot2')
    
  ci_cols = grep(pattern = paste0("_CI"), x = colnames(data), value = TRUE )

  ggplot(data = data, aes(x = StartTime, y = 0)) +
    geom_ribbon(aes_string(ymin=ci_cols[1], ymax=ci_cols[2]), alpha=0.5) +
    geom_hline(aes(yintercept = 0), linetype="dashed") +
    ylab("Parameter Estimate") +
    xlab("TimeBin (Start)") +
    facet_wrap( ~ AOI)
  
}

# Analyzing ------------------------------------------------------------------------------------------

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
    
  dots = list()
  for (i in seq_along(predictors)) {
    name = paste0(predictors[i], "C")
    dots[[name]] = make(nse_arg("as.numeric(", predictors[i], ") - mean(as.numeric(",  predictors[i], "), na.rm = TRUE)"))
  }

  data %>% 
    ungroup() %>%
    mutate_(.dots = dots)
  
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
# @param character.vector factors A vector of factor columns
# @param character summarize_by Should the data by summarized by participant, by item, or crossed (both)?
#
# @return dataframe summarized
time_analysis <- function (data, 
                           data_options, 
                           time_bin_size = 250, 
                           dv = data_options$default_dv, 
                           factors,
                           summarize_by = 'crossed') {
  
  
  require('dplyr')
  
  # For Multiple DVs:
  if (length(dv) > 1) {
    list_of_dfs = lapply(X = dv, FUN = function(this_dv) {
      cat("\nCreating Summary for", this_dv, "...")
      time_analysis(data, data_options, time_bin_size, this_dv, factors, summarize_by)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('time_analysis', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data)
  dopts = data_options
  
  # Divide Factors into Numeric, Factor:
  numeric_summarise_arg = list()
  condition_factors = c()
  for (factor in factors) {
    if ( is.numeric(data[[factor]]) ) {
      numeric_summarise_arg[[factor]] = make( nse_arg("mean(", factor, ", na.rm=TRUE)") )
    } else {
      if ( is.factor(data[[factor]]) ) {
        condition_factors = c(condition_factors, factor)
      } else {
        error('Your factor ', factor, ' must be of data-type "numeric" or "factor."')
      }
    }
  }
  
  # Create NSE Args:
  group_by_arg = switch(match.arg(summarize_by, c('crossed', 'subjects', 'participants', 'items')),
                        crossed      = as.list( c(dopts$participant_factor, dopts$item_factors, condition_factors, "TimeBin") ),
                        subjects     = as.list( c(dopts$participant_factor, condition_factors, "TimeBin") ),
                        participants = as.list( c(dopts$participant_factor, condition_factors, "TimeBin") ),
                        items        = as.list( c(dopts$item_factors, condition_factors, "TimeBin") )
                          )
  time_bin_arg = list(TimeBin = make(nse_arg("ceiling(", dopts$time_factor, "/", time_bin_size, ")")) )
  summarise_arg = c(numeric_summarise_arg,
                    list(PropLooking = make( nse_arg("mean(", dv, ", na.rm=TRUE)") ),
                         StartTime = make( nse_arg(data_options$time_factor, "[1]")),
                         EndTime = make( nse_arg( data_options$time_factor, "[n()]"))
                    )
  )
  
  # Make summarized dataframe:
  summarised = data %>%
    mutate_(.dots = time_bin_arg) %>%
    group_by_(.dots =  group_by_arg) %>%
    summarise_(.dots =  summarise_arg) %>%
    mutate(ArcSin = asin( sqrt( PropLooking ) ),
           AOI = dv) %>%
    ungroup()
  
  class(summarised) = c('time_analysis', class(summarised))
  
  summarised
    
}

# analyze_time_bins()
#
# Analyze bins sequentially looking for a main effect of a single factor.
# Uses the Arcsin-square root transformation on the DV.
#
# @param dataframe.time_analysis data The output of the 'time_analysis' function
# @param list data_options
# @param integer time_bin_size The time (ms) to fit into each bin
# @param string dv The dependent variable column
# @param character.vector condition_factor 
# @param character.vector factor_levels A vector of length two that specifies which factor-levels to compare 
#
# @return dataframe 
analyze_time_bins <- function(data, 
                                     data_options, 
                                     condition_factor = data_options$default_factors[1],
                                     factor_levels = NULL,
                                     return_full_model = FALSE)
{
  
  require('dplyr')
  require('lme4')
  require('broom')
  
  ## Helpers: ===
  lmer_helper = function(tb) {
    failsafe_lmer = function(formula, data) {
      tryCatch(lmer(formula, data),
               error = function(c) NA,
               warning = function(c) NA # return no model on convergence errors
      )
    }
    
    #failsafe_lmer = failwith(default = NA, lmer)
    failsafe_lmer(formula = as.formula(formula_string), data = filter(data, TimeBin==tb) )
  }
  extract_conf_ints = function(object, pnum, lh) {
    out = confint(object, method = "Wald")[pnum, lh]
    if (is.null(out)) {
      return(NA)
    } else {
      return(out)
    }
  }
  
  ## Main: ===
  
  # Must be a time_analysis:
  if (!'time_analysis' %in% class(data)) stop('This function can only be run on the output of the "time_analysis" function.')
  
  # For Multiple DVs:
  dvs = unique(data[['AOI']])
  if ( length(dvs) > 1 ) {
    list_of_dfs = lapply(X = dvs, FUN = function(this_dv) {
      cat("\nAnalyzing", this_dv, "...")
      this_df = filter(data, AOI == this_dv)
      class(this_df) = class(data)
      analyze_time_bins(data = this_df, data_options, condition_factor, factor_levels, return_full_model)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('seq_bin', class(out))
    return( out )
  }
  
  # Prelims:
  data = ungroup(data) # shouldn't be necessary, but just in case
  dopts = data_options
  
  if ( length(condition_factor) > 1 ) {
    stop('Sequential bins analysis can only be done on one factor at a time.')
  }
  if ( !is.numeric(data[[condition_factor]]) ) {
    num_levels = levels(data[[condition_factor]])
    if (num_levels == 0) {
      stop('Group factor supplied must be of data-type "numeric" or "factor."')
    }
    if (is.null(factor_levels)) {
      if (num_levels > 2) {
        stop("If factor has more than two levels, ",
             "you must specify which you are interested in comparing with 'factor_levels' arg.")
      } else {
        factor_levels = levels(data[[condition_factor]])
      }
    }
  }


  # Generate Model String:
  the_formula = list()
  the_formula[['main']] = paste0("ArcSin ~ ", condition_factor)
  if (dopts$participant_factor %in% colnames(data)) the_formula[['participant']] = paste0("(1|", data_options$participant_factor, ")")
  for (item in dopts$item_factors) {
    if (item %in% colnames(data)) {
      the_formula[[item]] = paste0("(1|", item, ")")
    }
  }
  formula_string = paste(the_formula, collapse = " + ")
  
  # Generate Models:
  LM_Model = lapply(X = unique(data$TimeBin), FUN = lmer_helper)
  models = data %>%
    group_by_(.dots = list("TimeBin") ) %>%
    summarise(StartTime = mean(StartTime)) %>%
    ungroup() %>%
    mutate(LM_Model = LM_Model)
  
  # Generate Output:
  out = select(models, -LM_Model)
  
  # Conf:
  out[[paste(condition_factor,"CILower",sep="_")]] = sapply(X = models$LM_Model, FUN = function(x) extract_conf_ints(x, 2, 1))
  out[[paste(condition_factor,"CIUpper",sep="_")]] = sapply(X = models$LM_Model, FUN = function(x) extract_conf_ints(x, 2, 2)) 
  
  # Diff:
  out$Difference = 
    (out[[paste(condition_factor,"CILower",sep="_")]] > 0) |
    (out[[paste(condition_factor,"CIUpper",sep="_")]] < 0) 
  
  # Tidy:
  if (return_full_model) {
    out$Model = models$LM_Model
  } else {
    out$Model = lapply(models$LM_Model, FUN = function(x) tidy(x, effects="fixed") ) 
  }
  
  # Return:
  out$AOI = unique(data$AOI)
  class(out) = c("seq_bin", class(out))
  out    
    
}

# window_analysis()
#
# Collapse time across our entire window and return a dataframe ready for LMERing
#
# @param dataframe data
# @param list data_options
# @param string dv
# @param character.vector condition_factors
# @param numeric.vector window
#
# @return dataframe
window_analysis <- function(data, 
                            data_options, 
                            dv = data_options$default_dv, 
                            condition_factors = data_options$default_factors,
                            window = NULL
                            ) {
  
  require('dplyr')
  
  # For Multiple DVs:
  if (length(dv) > 1) {
    list_of_dfs = lapply(X = dv, FUN = function(this_dv) {
      cat("\nAnalyzing", this_dv, "...")
      window_analysis(data, data_options, dv = this_dv, condition_factors, window)
    })
    out = bind_rows(list_of_dfs)
    class(out) = c('window_analysis', class(out))
    return( out )
  }
  
  # Make/Get Window:
  if ( is.null(window) ) {
    window = range(data[[data_options$time_factor]], na.rm = TRUE, finite = TRUE)
  }
  
  # Summarise:
  summarized = data %>% 
    filter_(.dots = list( paste0(data_options$time_factor, " >= ", window[1]),
                          paste0(data_options$time_factor, " <= ", window[2]))
    ) %>%
    group_by_(.dots = as.list(c(data_options$participant_factor, data_options$trial_factor, data_options$item_factors, condition_factors)) ) %>%
    summarise_( .dots = list(SamplesInAOI = make(nse_arg( "sum(", dv, ", na.rm= TRUE)" )),
                             SamplesTotal = make(nse_arg( "sum(!is.na(", dv, "))" )) # ignore trackloss!
                             ) ) %>%
    mutate(elog = log( (SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5) ) ,
           weights = 1 / ( ( 1 / (SamplesInAOI + .5) ) / ( 1 / (SamplesTotal - SamplesInAOI +.5) ) ),
           Prop = SamplesInAOI / SamplesTotal,
           ArcSin = asin( sqrt( Prop ) )
    )
  
  summarized$AOI = dv
  
  class(summarized) = c('window_analysis', class(summarized))
  return(summarized)
  
}

# Helpers -----------------------------------------------------------------------------------------------

nse_arg = function(..., cond=TRUE) {
  if (cond) {
    as.formula(paste0("~", ...))
  } else {
    "~NA"
  }
}

make = as.formula



