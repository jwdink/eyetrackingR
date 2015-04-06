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
# @created July 24, 2012
#

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
# 
# require(plyr)

set_data_options <- function(
                        sample_rate = 60,
                        participant_factor = 'ParticipantName',
                        active_aoi_factor = 'SceneType',
                        trackloss_factor = 'TrackLoss',
                        time_factor = 'TimeFromMovieOnset',
                        sample_factor = 'FramesFromMovieOnset',
                        trial_factor = 'Trial',
                        default_dv = 'ActionMatch',
                        default_factors = c('Condition')
                    ) {
  list(
            'sample_rate' = sample_rate,
            'participant_factor' = participant_factor,
            'active_aoi_factor' = active_aoi_factor,
            'trackloss_factor' = trackloss_factor,
            'time_factor' = time_factor,
            'sample_factor' = sample_factor,
            'trial_factor' = trial_factor,
            'default_dv' = default_dv,
            'default_factors' = default_factors
          )
}

# verify_dataset()
#
# Verify the status of the dataset by assessing columns in your data_options
#
# @param dataframe data
# @param list data_options
# @param boolean silent
#
# @return dataframe data

verify_dataset <- function(data, data_options, silent = FALSE) {
  
  out = data
  
  as.numeric2 = function(x) as.numeric(as.character(x))
  check_then_convert = function(x, checkfunc, convertfunc, colname) { 
    if ( !checkfunc(x) ) {
      warning('WARNING! ' , colname ,' column should be a factor. Check that the field was imported properly i.e. without empty rows.')
      convertfunc(x)
    } else {
      x
    }
  }
  col_type_converter = list("participant_factor" = function(x) check_then_convert(x, is.factor, as.factor, "Participants"),
                            "time_factor"        = function(x) check_then_convert(x, is.numeric, as.numeric2, "Time"),
                            "sample_factor"      = function(x) check_then_convert(x, is.numeric, as.numeric2, "Sample"),
                            "trial_factor"       = function(x) check_then_convert(x, is.factor, as.factor, "Trial")
                            )
  for (col in names(col_type_converter) ) {
    out[[ data_options[[col]] ]] = col_type_converter[[col]]( out[[ data_options[[col]] ]] )
  }
  
  return( out )
  
}

# # describe_data ()
# #
# # Describe a DV column in the dataset by a (group of) factor(s)
# #
# # @param dataframe data
# # @param string dv
# # @param character.vector factors
# #
# # @return null It prints to the screen
# describe_data <- function(data, dv, factors = c()) {
#   require('psych', quietly = TRUE)
#   
#   # check that factors exist
#   if (length(factors) == 0) {
#     error('describe_data','Factors argument missing.  Factors must be passed as charactero vector.')
#   }
#   
#   # get column names
#   columns <- colnames(data)
#   
#   describe_factors <- data.frame(matrix(ncol = length(factors), nrow = nrow(data)))
#   
#   for (i in 1:length(factors)) {
#     factor <- factors[i]
#     if (!1 %in% match(columns, factor)) {
#       error('describe_data',paste(factor, ' factor missing from dataset.  Each factor must have a column in the data object.',sep=""))
#     }
#     else if (!is.factor(data[, factor])) {
#       # factor it
#       data[, factor] <- factor(data[, factor])
#     }
#     
#     # add to dataframe
#     describe_factors[, i] <- data[, factor]
#   }
#   
#   cat(paste("DV: ",dv,"\n====================================================================================\n",sep=""))
#   
#   details <- by(data[, dv], describe_factors, describe_column)
#   details <- capture.output(print(details, quote = FALSE))
#   
#   # we captured the print() output above because the normal formatting of our details
#   # text is really off and I want it to look nice.  so we capture it and then manipulate
#   # it with regex.
#   details <- sub('(.*?)matrix\\.(.*?)\\:',' X1:',details)
#   details <- sub('\\[1\\]\\s+',"",details)
#   details <- sub('(-+)',"\n------------------------------------------------------------------------------------\n",details)
#   details <- sub('X([0-9]+)\\: ([[:alnum:]]+)',"X\\1: \\2\n",details)
#   details <- sub('\\n\\s+',"\n",details)
#   
#   cat(details)
#   
#   cat("\n\n")
# }
# 
# # describe_column()
# #
# # Supports describe_data(), above
# describe_column <- function (data) {
#   output <- paste("[Mean]: ",round(mean(data, na.rm = TRUE),3),"      ",
#                   "[SD]:   ",round(sd(data, na.rm = TRUE),3),"      ",
#                   "[Var]:  ",round(var(data, na.rm = TRUE),3),"      ",
#                   "[Min]:  ",round(min(data, na.rm = TRUE),3),"      ",
#                   "[Max]:  ",round(max(data, na.rm = TRUE),3),"      ",sep="")
#   
#   output
# }
# 
# # generate_random_data()
# #
# # Generate a random set of looking time data
# #
# # @param list data_options
# # @param integer seed (optional)
# #
# # @return dataframe data
# generate_random_data <- function (data_options, seed = NA, num_participants = 20) {
#   # if we have a seed, set it so that we can generate a consistent dataset
#   # perhaps to follow along in a tutorial
#   if (!is.na(seed)) {
#     set.seed(seed)
#   }
#   
#   # Experiment design
#   #
#   # Looking while listening, look to target
#   # 
#   # Will children look towards objects thematically associated with the heard word?
#   #
#   # Conditions:
#   #     Related, one of the objects is related to the word
#   #     NotRelated, no thematic matches
#   
#   # set participants
#   participants <- c(1:num_participants)
#   
#   # Rows
#   #
#   # We will have 8 seconds of total looking in each of our 6 trials for 20 participants
#   #   baseline: 4 seconds
#   #   response: 4 seconds
#   
#   num_rows <- data_options$sample_rate*8*6*max(participants)
#   
#   # Columns
#   #
#   # ParticipantName
#   # Age
#   # Condition
#   # TrialNum
#   # Trial
#   # Window
#   # Time
#   # Sample
#   # Trackloss
#   # ActiveAOI (Target, Distractor)
#   # Target
#   # Distractor
#   
#   data <- data.frame(matrix(nrow = num_rows, ncol = 12))
#   colnames(data) <- c(data_options$participant_factor, 'Age', 'Condition', 'TrialNum', data_options$trial_factor, 'Window', data_options$time_factor, data_options$sample_factor, data_options$trackloss_factor, data_options$active_aoi_factor, 'Target', 'Distractor')
#   
#   # calculate the general slopes by condition
#   
#   # break the response period into 16 phases, and use a log function to
#   # increase the likelihood of a look towards the Target
#   log_target <- rep(log(seq(1,16), 100000),each=((data_options$sample_rate*4)/16))
#   
#   # add some error
#   error <- rnorm((data_options$sample_rate*4),0,.15)
#   
#   # calculate looking to target (>.5 == a target look)
#   y <- .5 + log_target + error
#   
#   # fix ceiling/floor hits
#   y[which(y > 1.0)] <- 1.0
#   y[which(y < 0)] <- 0
#   
#   Related <- y
#   
#   # and again...
#   error <- rnorm((data_options$sample_rate*4),0,.07)
#   y <- .55 + error
#   y[which(y > 1.0)] <- 1.0
#   y[which(y < 0)] <- 0
#   
#   NotRelated <- y
#   
#   for (x in participants) {
#     # this participant's data will live in these rows
#     row_range <- seq((((x - 1)*(data_options$sample_rate*8*6))+1),(x*data_options$sample_rate*8*6))
#     
#     participant_id <- paste('SUBJ_',x,sep="")
#     condition <- ifelse(x <= floor(length(participants) / 2), 'Knower','NonKnower')
#     data[row_range, data_options$participant_factor] <- participant_id
#     data[row_range, 'Age'] <- round(rnorm(1,24,.25), 2)
#     data[row_range, 'Condition'] <- condition
#     data[row_range, 'TrialNum'] <- rep(c(1:6),each=(data_options$sample_rate*8))
#     data[row_range, data_options$trial_factor] <- rep(c('Chimpanzee','Bowl','Speaker','Woman','Pen','Basketball'),each=(data_options$sample_rate*8))
#     data[row_range, data_options$sample_factor] <- rep(c(1:(data_options$sample_rate*8)),times=6)
#     data[row_range, data_options$time_factor] <- (data[row_range, data_options$sample_factor]-1)*(1000/data_options$sample_rate)
#     
#     # generate trackloss probability for this participant
#     trackloss_probability <- runif(1,.1,0.35)
#     
#     data[row_range, data_options$trackloss_factor] <- rbinom((data_options$sample_rate*8*6),1,trackloss_probability)
#     
#     # for baseline, they have a 50% chance of looking to the target or distractor
#     data[row_range, data_options$active_aoi_factor] <- 0
#     
#     # AOI assignment is by trial
#     for (y in 1:6) {
#       # for baseline, let's just give everyone a 50/50 chance
#       baseline_range <- which(data[, data_options$participant_factor] == participant_id & data[, data_options$sample_factor] <= (data_options$sample_rate*4) & data[, 'TrialNum'] == y)
#       
#       data[baseline_range, 'Window'] <- 'Baseline'
#       data[baseline_range, 'Target'] <- ifelse(rbinom((data_options$sample_rate*4),1,0.5) == 1,1,0)
#       
#       # for response, we can set the target/distractor looks using the conditions
#       # sloped probability (defined above)
#       response_range <- which(data[, data_options$participant_factor] == participant_id & data[, data_options$sample_factor] > (data_options$sample_rate*4) & data[, 'TrialNum'] == y)
#       
#       data[response_range, 'Window'] <- 'Response'
#       
#       if (condition == 'Knower') {
#         data[response_range, 'Target'] <- Related
#       }
#       else {
#         data[response_range, 'Target'] <- NotRelated
#       }
#       
#       data[response_range, 'Target'] <- sapply(data[response_range, 'Target'], function (x) {
#         rbinom(1,1,as.numeric(x))
#       });
#     }
#     
#     # set AOI columns from active_aoi_factor
#     data[which(data[, 'Target'] == 1), data_options$active_aoi_factor] <- 'Target'
#     data[which(data[, 'Target'] == 1), 'Distractor'] <- 0
#     data[which(data[, 'Target'] == 0), data_options$active_aoi_factor] <- 'Distractor'
#     data[which(data[, 'Target'] == 0), 'Distractor'] <- 1
#     
#     # no AOI for trackloss
#     data[which(data[, data_options$trackloss_factor] == 1), data_options$active_aoi_factor] <- NA
#     data[which(data[, data_options$trackloss_factor] == 1), 'Target'] <- NA
#     data[which(data[, data_options$trackloss_factor] == 1), 'Distractor'] <- NA
#   }
#   
#   # verify and format dataset
#   data <- verify_dataset(data, data_options, silent = TRUE)
#   
#   data$Condition <- factor(data$Condition)
#   data$Window <- factor(data$Window)
#   data[, data_options$active_aoi_factor] <- factor(data[, data_options$active_aoi_factor])
#   
#   data
# }
# 
# # clean_by_factor()
# #
# # Clean a dataset by only retaining rows that match one of several values
# # on a specified factor. For example, you may have a column of MovieName and
# # only want to retain timeperiods where the participant was watching "TestMovie.mov".
# # Or, you may have a column Phase and only be interested in the "Baseline" and
# # "Test" phases.
# #
# # @param dataframe data; e.g., master_clean
# # @param list data_options, e.g., data_options
# # @param string factor; e.g., 'MovieName'
# # @param character.vector allowed_levels; e.g., c('Movie1','Movie2')
# #
# # @return dataframe data
# clean_by_factor <- function (data, data_options, factor, allowed_levels) {
#   # refactor column to ensure it is indeed a factor
#   data[, factor] <- factor(data[, factor])
#   
#   # subset data by rows that meet the criterion
#   data <- subset(data, data[,factor] %in% allowed_levels)
# 
#   data
# }
# 
# # treat_outside_looks_as_trackloss()
# #
# # Treat looks outside of any AOIs as trackloss.
# #
# # @param dataframe data
# # @param list data_options
# #
# # @return dataframe data
# treat_outside_looks_as_trackloss <- function(data, data_options) {
#   # if the active AOI column is empty or NA, set the trackloss column as 1
#   if (length(which(data[, data_options$active_aoi_factor] == '')) > 0) {
#     data[which(data[, data_options$active_aoi_factor] == ''), data_options$trackloss_factor] <- 1
#   }
#   
#   data[is.na(data[, data_options$active_aoi_factor]), data_options$trackloss_factor] <- 1
# 
#   data
# }
# 
# # get_trackloss_data()
# #
# # Describe the type of trackloss in our data or, more specifically, within a subsetted time
# # window of our data.
# #
# # @param dataframe data
# # @param list data_options
# # @param integer window_start optional
# # @param integer window_end optional
# #
# # @return list(trackloss, trials_committed)
# get_trackloss_data <- function(data, data_options, window_start = NA, window_end = NA) {
#   # subset data by the optional window parameters
#   data <- subset_by_window(data, data_options, window_start, window_end)
#   
#   # count looking frames by trials/babies
#   looking <- aggregate(data[, data_options$trackloss_factor], by = list(data[, data_options$participant_factor], data[, data_options$trial_factor]), 'sum', na.rm = TRUE)
#   
#   # count total datapoints by trials/babies
#   total <- aggregate(data[, data_options$participant_factor], by = list(data[, data_options$participant_factor],data[, data_options$trial_factor]), 'length')
#   looking$TotalFrames <- total[, 3]
#   
#   colnames(looking) <- c(data_options$participant_factor, data_options$trial_factor,'SamplesLooking','TotalSamples')
#   
#   # now flip the SamplesLooking column, as it currently counts the trackloss (not good frames)
#   looking$SamplesLooking <- looking$TotalSamples - looking$SamplesLooking
#   
#   # add new columns for convenience
#   looking$TracklossSamples <- looking$TotalSamples - looking$SamplesLooking
#   looking$TracklossProportion <- looking$TracklossSamples / (looking$TotalSamples)
#   
#   # count trials by babies to see which were excluded just by not finishing the experiment
#   # or not looking AT ALL (prior to trackloss scrubbing)
#   looking_no_zeroes <- looking[which(looking$SamplesLooking > 0), ]
#   trials_committed <- aggregate(looking_no_zeroes[, data_options$trial_factor], by = list(looking_no_zeroes[, data_options$participant_factor]), 'length')
#   colnames(trials_committed) <- c(data_options$participant_factor,'Trials')
#   
#   list (
#       trackloss = looking,
#       trials_committed = trials_committed
#     )
# }
# 
# # clean_by_trackloss()
# #
# # Clean our master file by removing data from trials that contain less than a
# # specified number of non-trackloss samples.
# #
# # @param dataframe data
# # @param list data_options
# # @param integer window_start optional
# # @param integer window_end optional
# # @param integer minimum_samples
# #
# # @return dataframe data
# clean_by_trackloss <- function(data, data_options, window_start = NA, window_end = NA, minimum_samples = NA) {
#   # copy this object as we will want to manipulate it later.
#   # we are going to cut the original data by the window dimensions but
#   # want to return the full dataset.
#   raw_data <- data
#   
#   # subset data by the optional window parameters
#   data <- subset_by_window(data, data_options, window_start, window_end)
#   
#   looking <- aggregate(data[, data_options$trackloss_factor], by = list(data[, data_options$participant_factor],data[, data_options$trial_factor]), 'sum', na.rm = TRUE)
#   
#   # count total datapoints by trials/babies
#   total <- aggregate(data[, data_options$participant_factor], by = list(data[, data_options$participant_factor],data[, data_options$trial_factor]), 'length')
#   looking$TotalSamples <- total[, 3]
#   
#   colnames(looking) <- c(data_options$participant_factor,data_options$trial_factor,'SamplesLooking','TotalSamples')
#   
#   # now flip the SamplesLooking column, as it currently counts the trackloss (not good samples)
#   looking$SamplesLooking <- looking$TotalSamples - looking$SamplesLooking
#   
#   # which should be removed?
#   message('clean_by_trackloss','Calculating trials to be dropped...')
#   trackloss_trials <- looking[which(looking$SamplesLooking < minimum_samples), ]
#   
#   message('clean_by_trackloss',paste('Dropping ',nrow(trackloss_trials),' trials...',sep=""))
#   
#   if (nrow(trackloss_trials) > 0) {
#     for (i in 1:nrow(trackloss_trials)) {
#       row <- trackloss_trials[i, ]
#       
#       raw_data <- raw_data[-which(raw_data[, data_options$trial_factor] == as.character(row[, data_options$trial_factor]) & raw_data[, data_options$participant_factor] == as.character(row[, data_options$participant_factor])), ]
#     }
#   }
#   
#   raw_data
# }
# 
# # final_trial_counts()
# #
# # Calculate how many trials were committed to our analysis by each participant
# #
# # @param dataframe data
# # @param list data_options
# # @param integer window_start
# # @param integer window_end
# #
# # @return dataframe trials_committed
# final_trial_counts <- function(data, data_options, window_start = NA, window_end = NA) {
#   # subset data by the optional window parameters
#   data <- subset_by_window(data, data_options, window_start, window_end)
#   
#   excluding_trackloss <- data[which(data[, data_options$trackloss_factor] == 0 | is.na(data[, data_options$trackloss_factor])), ]
#   
#   # count looking frames by trials/babies
#   looking <- aggregate(excluding_trackloss[, data_options$participant_factor], by = list(excluding_trackloss[, data_options$participant_factor],excluding_trackloss[, data_options$trial_factor]), 'length')
#   colnames(looking) <- c(data_options$participant_factor,data_options$trial_factor,'SamplesLooking')
#   
#   # count trials by babies
#   trials_committed <- aggregate(looking[, data_options$trial_factor], by = list(looking[, data_options$participant_factor]), 'length')
#   colnames(trials_committed) <- c(data_options$participant_factor,'Trials')
#   
#   trials_committed
# }
# 
# # keep_trackloss()
# #
# # Keep individual trackloss points by making sure all AOI
# # columns are accurate.
# #
# # @param dataframe data
# # @param list data_options
# #
# # @return dataframe data
# keep_trackloss <- function(data, data_options) {
#   scenes <- levels(data[, data_options$active_aoi_factor])
#   
#   # the idea here is to set all AOI columns to 0 where they are currently NA
#   # so that we can get accurate proportions across the column.
#   # if we didn't do this, we would get proportions that exclude the NA rows
#   # (as if we were excluding trackloss)
#   for (i in 1:length(scenes)) {
#     scene <- scenes[i]
#     
#     if (scene != '') {     
#       if (length(data[is.na(data[,scene]), scene]) > 0) {
#         data[is.na(data[,scene]), scene] <- 0
#       }
#     }
#   }
#   
#   data
# }
# 
# # remove_trackloss()
# #
# # Remove individual trackloss points by removing all rows with the
# # trackloss column set as 1.
# #
# # @param dataframe data
# # @param list data_options
# # 
# # @return dataframe data
# remove_trackloss <- function(data, data_options) {
#   data <- data[-which(data[, data_options$trackloss_factor] == 1), ]
#   
#   data
# }
# 
# # time_analysis()
# #
# # Create a dataset that is ready for time-based analyses, either a linear
# # analysis or a non-linear (i.e., growth curve) analysis
# #
# # @param dataframe data
# # @param list data_options
# # @param integer bin_time The time (ms) to fit into each bin
# # @param string dv The dependent variable column
# # @param character.vector A vector of factor columns
# #
# # @return list(bySubj, byItem, crossed)
# time_analysis <- function (data, data_options, bin_time = 250, dv = NA, factors = NA) {
#   # use defaults if unset
#   if (is.na(dv)) {
#     dv = data_options$default_dv
#   }
#   
#   if (length(factors) < 2 && is.na(factors)) {
#     factors = data_options$default_factors
#   }
#   
#   # make sure the factor columns are actually factors for the sake of binning
#   # but track the classes so we can reset these afterwards...
#   original_classes = list()
#   
#   for (i in 1:length(factors)) {
#     factor_name = factors[i]
#     
#     # save original class
#     # if its character/factor, we will leave as a factor later
#     # if its numeric, we will return it to its numeric state
#     original_classes[factor_name] = class(data[, factor_name])
#     
#     # now factor!
#     data[, factor_name] <- factor(data[, factor_name])
#   }
#   
#   # ====== create bySubj data frame
#   
#   # calculate the number of samples to place in a bin based on
#   # the amount of time (ms) we want in a bin (bin_time)
#   bin_size_in_samples <- round(bin_time / (1000 / data_options$sample_rate),0)
#   
#   # bin by participant and factors across time
#   binned <- bin_data(data, data_options, bin_size_in_samples, c(data_options$participant_factor,factors), dv)
#   
#   # rename columns
#   colnames(binned) <- c(data_options$participant_factor,factors,'Bin','BinMean','N','y')
#   
#   # because we may have excluded trial time before our window, let's re-calibrate
#   # our bin numbers to a zero point
#   min_bin <- min(binned[, 'Bin'])
#   
#   if (min_bin > 0) {
#     binned$Bin <- binned$Bin - min_bin
#   }
#   
#   # create t1
#   binned$t1 <- binned$Bin * (bin_time / 1000)
#   
#   # rename as bySubj
#   bySubj <- binned
#   
#   # are we fitting an empirical logit manual
#   bySubj$elog <- log( (bySubj$y + .5) / (bySubj$N - bySubj$y + .5) )
#   bySubj$wts <- 1 / (bySubj$y + .5) + 1 / (bySubj$N - bySubj$y + .5)
#   
#   # create ArcSin column
#   bySubj$ArcSin <- asin(sqrt(bySubj$BinMean))
#   
#   # create an "aggregate" column so we can uniquely code each instance of our factors
#   bySubj$AggID <- 0
#   
#   last_row <- NA
#   
#   for (i in 1:nrow(bySubj)) {
#     row <- bySubj[i, ]
#     
#     if (length(last_row) > 1) {
#       for (j in 1:length(factors) + 1) {
#         if (row[, j] != last_row[, j]) {
#           bySubj[i, 'AggID'] <- max(bySubj$AggID) + 1
#         }
#       }
#       
#       bySubj[i, 'AggID'] <- max(bySubj$AggID)
#     }
#     else {
#       bySubj[i, 'AggID'] <- 1
#     }
#     
#     last_row <- row
#   }
#   
#   # factor this column
#   bySubj$AggID <- factor(bySubj$AggID)
#   
#   # generate orthogonal polynomial linearized time correlates
#   # generate polygonal time correlates
#   t <- poly((unique(bySubj$Bin)), 4)
#   
#   bySubj$ot1 <- 0
#   bySubj$ot2 <- 0
#   bySubj$ot3 <- 0
#   bySubj$ot4 <- 0
#   
#   for (i in 1:nrow(t)) {
#     bySubj[which(bySubj$Bin == (i - 1)),"ot1"] <- t[i, 1]
#     bySubj[which(bySubj$Bin == (i - 1)),"ot2"] <- t[i, 2]
#     bySubj[which(bySubj$Bin == (i - 1)),"ot3"] <- t[i, 3]
#     bySubj[which(bySubj$Bin == (i - 1)),"ot4"] <- t[i, 4]
#   }
#   
#   # ====== create byItem data frame
#   
#   # calculate the number of samples to place in a bin based on
#   # the amount of time (ms) we want in a bin (bin_time)
#   bin_size_in_samples <- round(bin_time / (1000 / data_options$sample_rate),0)
#   
#   # bin by participant and factors across time
#   binned <- bin_data(data, data_options, bin_size_in_samples, c(data_options$participant_factor,factors), dv)
#   
#   # rename columns
#   colnames(binned) <- c(data_options$trial_factor,factors,'Bin','BinMean','N','y')
#   
#   # because we may have exclude trial time before our window, let's adjust our bin numbers
#   # so they start at 0
#   min_bin <- min(binned[, 'Bin'])
#   
#   if (min_bin > 0) {
#     binned$Bin <- binned$Bin - min_bin
#   }
#   
#   # create t1
#   binned$t1 <- binned$Bin * (bin_time / 1000)
#   
#   # rename as byItem
#   byItem <- binned
#   
#   # are we fitting an empirical logit manual
#   byItem$elog <- log( (byItem$y + .5) / (byItem$N - byItem$y + .5) )
#   byItem$wts <- 1 / (byItem$y + .5) + 1 / (byItem$N - byItem$y + .5)
#   
#   # create ArcSin column
#   byItem$ArcSin <- asin(sqrt(byItem$BinMean))
#   
#   # create an "aggregate" column so we can uniquely code each instance of Condition:Trial
#   byItem$AggID <- 0
#   
#   last_row <- NA
#   
#   for (i in 1:nrow(byItem)) {
#     row <- byItem[i, ]
#     
#     if (length(last_row) > 1) {
#       for (j in 1:length(factors) + 1) {
#         if (row[, j] != last_row[, j]) {
#           byItem[i, 'AggID'] <- max(byItem$AggID) + 1
#         }
#       }
#       
#       byItem[i, 'AggID'] <- max(byItem$AggID)
#     }
#     else {
#       byItem[i, 'AggID'] <- 1
#     }
#     
#     last_row <- row
#   }
#   
#   # factor this column
#   byItem$AggID <- factor(byItem$AggID)
#   
#   # generate orthogonal polynomial linearized time correlates
#   # generate polygonal time correlates
#   t <- poly((unique(byItem$Bin)), 4)
#   
#   byItem$ot1 <- 0
#   byItem$ot2 <- 0
#   byItem$ot3 <- 0
#   byItem$ot4 <- 0
#   
#   for (i in 1:nrow(t)) {
#     byItem[which(byItem$Bin == (i - 1)),"ot1"] <- t[i, 1]
#     byItem[which(byItem$Bin == (i - 1)),"ot2"] <- t[i, 2]
#     byItem[which(byItem$Bin == (i - 1)),"ot3"] <- t[i, 3]
#     byItem[which(byItem$Bin == (i - 1)),"ot4"] <- t[i, 4]
#   }
#   
#   # ====== create crossed (by Items and Subjects) data frame
#   
#   # calculate the number of samples to place in a bin based on
#   # the amount of time (ms) we want in a bin (bin_time)
#   bin_size_in_samples <- round(bin_time / (1000 / data_options$sample_rate),0)
#   
#   # bin by participant and factors across time
#   binned <- bin_data(data, data_options, bin_size_in_samples, c(data_options$participant_factor,data_options$trial_factor,factors), dv)
#   
#   # rename columns
#   colnames(binned) <- c(data_options$participant_factor, data_options$trial_factor,factors,'Bin','BinMean','N','y')
#   
#   # because we may have exclude trial time before our window, let's adjust our bin numbers
#   # so they start at 0
#   min_bin <- min(binned[, 'Bin'])
#   
#   if (min_bin > 0) {
#     binned$Bin <- binned$Bin - min_bin
#   }
#   
#   # create linear time column
#   binned$t1 <- binned$Bin * (1000 / bin_time)
#   
#   # are we fitting an empirical logit manual
#   binned$elog <- log( (binned$y + .5) / (binned$N - binned$y + .5) )
#   binned$wts <- 1 / (binned$y + .5) + 1 / (binned$N - binned$y + .5)
#   
#   # create ArcSin column
#   binned$ArcSin <- asin(sqrt(binned$BinMean))
#   
#   # create an "aggregate" column so we can uniquely code each instance of Condition:Trial
#   binned$AggID <- 0
#   
#   last_row <- NA
#   
#   for (i in 1:nrow(byItem)) {
#     row <- binned[i, ]
#     
#     if (length(last_row) > 1) {
#       for (j in 1:length(factors) + 1) {
#         if (row[, j] != last_row[, j]) {
#           binned[i, 'AggID'] <- max(binned$AggID) + 1
#         }
#       }
#       
#       binned[i, 'AggID'] <- max(binned$AggID)
#     }
#     else {
#       binned[i, 'AggID'] <- 1
#     }
#     
#     last_row <- row
#   }
#   
#   # factor this column
#   binned$AggID <- factor(binned$AggID)
#   
#   # generate orthogonal polynomial linearized time correlates
#   # generate polygonal time correlates
#   t <- poly((unique(binned$Bin)), 4)
#   
#   binned$ot1 <- 0
#   binned$ot2 <- 0
#   binned$ot3 <- 0
#   binned$ot4 <- 0
#   
#   for (i in 1:nrow(t)) {
#     binned[which(binned$Bin == (i - 1)),"ot1"] <- t[i, 1]
#     binned[which(binned$Bin == (i - 1)),"ot2"] <- t[i, 2]
#     binned[which(binned$Bin == (i - 1)),"ot3"] <- t[i, 3]
#     binned[which(binned$Bin == (i - 1)),"ot4"] <- t[i, 4]
#   }
#   
#   # rename as crossed
#   crossed <- binned
#   
#   # ======== reset original column classes, if possible
#   
#   # above, we recorded original factor classes in 'original_classes'
#   # if these are numeric, let's reset them
#   # if they were characters/factors, we can leave them as factors for the sake
#   # of analysis
#   
#   for (factor_name in names(original_classes)) {
#     original_class <- original_classes[[factor_name]]  
#     if (original_class == 'numeric') {
#       bySubj[, factor_name] <- as.numeric(as.character(bySubj[, factor_name]))
#       byItem[, factor_name] <- as.numeric(as.character(byItem[, factor_name]))
#       crossed[, factor_name] <- as.numeric(as.character(crossed[, factor_name]))
#     }  
#     else {
#       bySubj[, factor_name] <- factor(bySubj[, factor_name])
#       byItem[, factor_name] <- factor(byItem[, factor_name])
#       crossed[, factor_name] <- factor(crossed[, factor_name])
#     }
#   }
#   
#   list (
#       'bySubj' = bySubj,
#       'byItem' = byItem,
#       'crossed' = crossed
#     )
# }
# 
# # window_analysis()
# #
# # Collapse time across our entire window and do an analyis with subjects and items as random effects
# #
# # @param dataframe data
# # @param list data_options
# # @param string dv
# # @param character.vector factors
# #
# # @return dataframe window_data
# window_analysis <- function(data, data_options, dv = NA, factors = NA) {
#   # use defaults if unset
#   if (is.na(dv)) {
#     dv = data_options$default_dv
#   }
#   
#   if (length(factors) < 2 && is.na(factors)) {
#     factors = data_options$default_factors
#   }
#   
#   # track the original column classes so we can reset these afterwards...
#   original_classes = list()
#   
#   for (i in 1:length(factors)) {
#     factor_name = factors[i]
#     
#     # save original class
#     original_classes[factor_name] = class(data[, factor_name])
#   }
#   
#   # set all rows of the var that are NA to 0, so we don't get errors in calculating a mean
#   # if we have rows like this, we have to assume they meant to keep trackloss, even though
#   # keep_trackloss() would have already set NA to 0...
#   data[is.na(data[, dv]), dv] <- 0
#   
#   # collapse across trials within subjects
#   test_sum <- aggregate(data[dv], by = data[c(data_options$participant_factor,factors, data_options$trial_factor)], FUN = 'sum')
#   test_length <- aggregate(data[dv], by = data[c(data_options$participant_factor, factors, data_options$trial_factor)], FUN = 'length')
#   
#   # transform our collapsed dataset into something pretty
#   test <- data.frame(test_sum[c(data_options$participant_factor,data_options$trial_factor,factors)], test_sum[dv], test_length[dv])
#   
#   colnames(test) <- c(data_options$participant_factor,data_options$trial_factor,factors,'y','N')
#   
#   # calculate elog, wts
#   test$elog <- log( (test$y + .5) / (test$N - test$y + .5) )
#   test$wts <- (1/(test$y + .5)) + (1/(test$N - test$y + .5))
#   
#   # calculate proportion
#   test$Proportion <- test$y / test$N
#   
#   # do an arcsine root transformation
#   test$ArcSin <- asin(sqrt(test$Proportion))
#   
#   # set column modes
#   test[, data_options$participant_factor] <- factor(test[, data_options$participant_factor])
#   test[, data_options$trial_factor] <- factor(test[, data_options$trial_factor])
#   
#   # above, we recorded original factor classes in 'original_classes'
#   # if these are numeric, let's reset them
#   # if they were characters/factors, we can set them as factors for the sake
#   # of analysis
#   
#   for (factor_name in names(original_classes)) {
#     original_class <- original_classes[[factor_name]]  
#     if (original_class == 'numeric') {
#       test[, factor_name] <- as.numeric(as.character(test[, factor_name]))
#     }  
#     else {
#       test[, factor_name] <- factor(test[, factor_name])
#     }
#   }
#   
#   test
# }
# 
# # sequential_bins_analysis()
# #
# # Analyze bins sequentially looking for a main effect of a single factor.
# # Uses the Arcsin-square root transformation on the DV.
# #
# # @param dataframe data
# # @param list data_options
# # @param integer bin_time The time (ms) to fit into each bin
# # @param string dv The dependent variable column
# # @param character.vector A vector of factor columns
# # @param boolean within_subjects
# #
# # @return dataframe window_data
# sequential_bins_analysis <- function(data, data_options, bin_time = 250, dv = NA, factor, within_subjects = FALSE) {
#   # use defaults if unset
#   if (is.na(dv)) {
#     dv = data_options$default_dv
#   }
#   
#   # calculate the number of samples to place in a bin based on
#   # the amount of time (ms) we want in a bin (bin_time)
#   bin_size_in_samples <- round(bin_time / (1000 / data_options$sample_rate),0)
#   
#   # bin by participant
#   binned <- bin_data(data, data_options, bin_size_in_samples, c(data_options$participant_factor,factor), dv)
#   
#   # rename columns
#   colnames(binned) <- c(data_options$participant_factor,factor,'Bin','BinMean','N','y')
#   
#   # because we may have exclude trial time before our window, let's adjust our bin numbers
#   # so they start at 0
#   min_bin <- min(binned[, 'Bin'])
#   
#   if (min_bin > 0) {
#     binned$Bin <- binned$Bin - min_bin
#   }
#   
#   # create ArcSin column if we are doing proportions
#   if (min(binned$BinMean >= 0) && max(binned$BinMean) <= 1.0) {
#     binned$ArcSin <- asin(sqrt(binned$BinMean))
#     cat("Transforming with Arcsine...\n")
#   }
#   else {
#     binned$ArcSin <- binned$BinMean
#     cat("Not transforming - does not appear to be proportional data...")
#   }
#   
#   # test each consecutive bin
#   bins <- max(binned$Bin)
#   
#   results <- data.frame(matrix(nrow = bins, ncol = 9))
#   colnames(results) <- c('Bin','StartTime','EndTime','Participants','SameDifferent','p-value','df_num','df_den','F')
#   
#   for (bin in 0:bins) {
#     # subset data into bin
#     bin_data <- binned[which(binned$Bin == bin), ]
#     
#     # re-factor the column
#     bin_data[, factor] <- factor(bin_data[, factor])
#     
#     # make sure that we have at least 2 levels of the factor
#     # in this bin
#     num_levels <- length(levels(bin_data[, factor]))
#   
#     if (num_levels <= 1) {
#       next
#     }
#     
#     frm <- paste('ArcSin',factor,sep=" ~ ")
#     
#     if (within_subjects == TRUE) {
#       frm <- paste(frm, ' + Error(ParticipantName)', sep='')
#     }
#     
#     model <- aov(formula(frm), data = bin_data)
#     
#     if (!is.numeric(summary(model)[[1]][["Pr(>F)"]][1]) && !is.numeric(summary(model)[[1]][[1]][["Pr(>F)"]][1])) {
#       # no p-value available
#       next
#     }
#     else if (within_subjects == TRUE) {
#       p_value <- round(summary(model)[[1]][[1]][["Pr(>F)"]][[1]],3)
#       df_num <- summary(model)[[1]][[1]][['Df']][[1]]
#       df_den <- summary(model)[[1]][[1]][['Df']][[2]]
#       F_value <- summary(model)[[1]][[1]][['F value']][[1]]
#     }
#     else {
#       p_value <- round(summary(model)[[1]][["Pr(>F)"]][[1]],3)
#       df_num <- summary(model)[[1]][['Df']][[1]]
#       df_den <- summary(model)[[1]][['Df']][[2]]
#       F_value <- summary(model)[[1]][['F value']][[1]]
#     }
#     
#     if (p_value == 0) {
#       p_value <- .001
#     }
#     
#     if (p_value <= .05) {
#       samedifferent <- 'different'
#     }
#     else {
#       samedifferent <- 'same'
#     }
#     
#     participants <- length(levels(factor(bin_data[, data_options$participant_factor])))
#     
#     results[bin + 1, ] <- c(bin, round((bin*bin_size_in_samples*(1000/data_options$sample_rate)),0), round(((bin*bin_size_in_samples + bin_size_in_samples)*(1000/data_options$sample_rate)),0), participants, samedifferent, p_value, df_num, df_den, F_value)
#   }
#   
#   results
# }
# 
# # first_looks_analysis()
# #
# # Look at an analysis of switches/stays on a dv AOI based on a subject's
# # first AND second looks. You can see if subjects are faster to switch
# # from AOI X to AOI Y than the reverse (implying they are moving nonrandomly
# # towards AOI Y)
# #
# # @param dataframe data
# # @param list data_options
# # @param character.vector factors
# #
# # @return dataframe first_looks
# first_looks_analysis <- function(data, data_options, factors = NA) {
#   # use defaults if unset
#   if (length(factors) < 2 && is.na(factors)) {
#     factors = data_options$default_factors
#   }
#   
#   # get the looks dataframe
#   looks <- get_looks(data, data_options, factors = c(factors))
#   
#   # first, let's clean up the looks dataframe
#   
#   # aggregate by participants > trials > AOIs and take the
#   # MIN(frames) values for each scenetype
#   
#   looks <- ddply(looks, c(data_options$participant_factor, factors, data_options$trial_factor, data_options$active_aoi_factor), summarise, StartTime = min(StartTime), EndTime = min(EndTime))
# 
#   # re-sort
#   sort_names <- c(data_options$participant_factor, factors, data_options$trial_factor, "StartTime", "EndTime")
#   looks <- looks[do.call("order", looks[sort_names]), ]
#   
#   # add the FixationNum column
#   looks$RankSequence <- 0
#   
#   max_fixations <- length(unique(looks[, data_options$active_aoi_factor]))
#   fixation_vector <- c(1:max_fixations)
#   
#   subjectnums <- unique(looks[, data_options$participant_factor])
#   trials      <- unique(looks[, data_options$trial_factor])
#   scenetypes  <- unique(looks[, data_options$active_aoi_factor])
#   
#   for (subjectnum in subjectnums) {
#     for (factor in factors) {
#       for (factor_level in unique(looks[, factor])) {
#         for (trial in trials) {
#           rows <- length(looks[which(looks[, data_options$participant_factor] == subjectnum & looks[, factor] == factor_level & looks[, data_options$trial_factor] == trial), 'RankSequence'])
#           
#           if (rows > 0) {
#             looks[which(looks[, data_options$participant_factor] == subjectnum & looks[, factor] == factor_level & looks[, data_options$trial_factor] == trial), 'RankSequence'] <- fixation_vector[0:rows]
#           }
#         }
#       }
#     }
#   }
#   
#   # get all first looks
#   index <- length(factors)
#   first_looks <- looks[which(looks$RankSequence == 1), 1:(4+index)]
#   colnames(first_looks)[3+index] <- 'FirstAOI'
#   colnames(first_looks)[4+index] <- 'FirstStartTime'
#   
#   # get all second looks
#   second_looks <- looks[which(looks$RankSequence == 2), 1:(5+index)]
#   colnames(second_looks)[3+index] <- 'SecondAOI'
#   colnames(second_looks)[4+index] <- 'SecondStartTime'
#   colnames(second_looks)[5+index] <- 'SecondEndTime'
#   
#   # merge first and second looks, getting rid of non-switch trials
#   merged <- merge(first_looks, second_looks, by = c(colnames(first_looks)[1:(2+index)]))
#   
#   # calculate switch time
#   merged$SwitchTime <- merged$SecondStartTime - merged$FirstStartTime
#   
#   # fixed columns
#   merged[, data_options$participant_factor] <- factor(merged[, data_options$participant_factor])
#   
#   merged
# }
# 
# # bootstrapped_splines(data, data_options, factor, within_subj, samples, resolution, alpha)
# #
# # Bootstrap splines from a time_analysis() file. Returns a plottable time series dataframe with means for each
# # level of Factor (i.e., each condition), as well as a time series dataframe which shows the points of divergence
# # throughout the time window.
# #
# # @param dataframe data Your clean dataset
# # @param list data_options Standard list of options for manipulating dataset
# # @param string factor What factor to split by? Maximum two conditions!
# # @param boolean within_subj Are the two conditions within or between subjects?
# # @param int samples How many (re)samples to take?
# # @param float resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100 intervals per second, or hundredths of a second
# # @param float alpha p-value when the groups are sufficiently "diverged"
# #
# # @return list(samples, divergence)
# bootstrapped_splines <- function (data, data_options, factor = '', within_subj = FALSE, samples=1000, resolution=10, alpha = .05) {
#   # dependencies
#   library(dplyr, quietly=TRUE)
#   library(data.table, quietly=TRUE)
#   
#   # define sampler for splines...
#   spline_sampler <- function (dataframe, data_options, resolution) {
#     if (rbinom(1,1,.1) == 1) {
#       cat('.')
#     }
#     
#     run_original <- dataframe
#     
#     # get subjects
#     run_subjects <- levels(run_original[, data_options$participant_factor])
#     
#     # get timepoints
#     run_times <- unique(run_original$t1)
#     run_times <- run_times[order(run_times)]
#     
#     # randomly sample N subjects (with replacement from data)
#     run_sampled <- sample(run_subjects, length(run_subjects), replace = TRUE)
#     
#     # create a dataset of ParticipantName,t1,BinMean for each sampled subject (including duplicates)
#     run_rows <- length(run_sampled) * length(run_times)
#     run_data <- data.frame(matrix(nrow=run_rows,ncol=2))
#     
#     # use data.table's setnames for speed increase
#     #colnames(run_data) <- c(data_options$participant_factor,'t1')
#     setnames(run_data,c(data_options$participant_factor,'t1'))
#     
#     run_data[, data_options$participant_factor] <- rep(run_sampled, each=length(run_times))
#     run_data[, data_options$participant_factor] <- factor(run_data[, data_options$participant_factor])
#     
#     run_data$t1 <- rep(run_times, times=length(run_subjects))
#     
#     # replaced merge with inner_join() for speed increase
#     #run_data <- merge(run_data, run_original[, c(data_options$participant_factor,'t1','BinMean')], by = c(data_options$participant_factor,'t1'))
#     run_data <- inner_join(run_data, run_original[, c(data_options$participant_factor,'t1','BinMean')], by = c(data_options$participant_factor,'t1'))
#     
#     # spline! 
#     # with generalized cross-validation setting smoothing parameter
#     run_spline <- with(run_data, smooth.spline(t1, BinMean, cv=FALSE))
#     
#     # get interpolated spline predictions for total time at .01 (hundredth of a second scale)
#     run_predicted_times <- seq(min(run_times), max(run_times), by=resolution)
#     run_predictions <- predict(run_spline, run_predicted_times)
#     
#     run_predictions$y
#   }
#   
#   # process arguments
#   
#   # convert resolution to seconds
#   resolution = resolution / 1000;
#   
#   # validate variables
#   if (factor == '') {
#     error('bootstrapped_splines','No factor specified. Must specify a factor with 2 levels.')
#   }
#   
#   if (length(levels(data[, factor])) != 2) {
#     error('bootstrapped_splines','Factor must have 2 levels.')
#   }
#   
#   # this dataframe will hold our final dataset
#   combined_bootstrapped_data <- data.frame()
#   
#   # re-factor Participant name column, so that levels() is accurate
#   data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
#   
#   # between-subjects:
#   if (within_subj == FALSE) {
#     for (label in levels(data[, factor])) {
#       subsetted_data <- data[which(data[, factor] == label), ]
#       
#       cat('Sampling')
#       bootstrapped_data <- replicate(samples, spline_sampler(subsetted_data, data_options, resolution))
#       bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))
#       
#       sample_rows <- paste('Sample', c(1:samples), sep="")
#       
#       # use data.table's setnames for speed
#       #colnames(bootstrapped_data) <- sample_rows
#       setnames(bootstrapped_data, sample_rows)
#       
#       bootstrapped_data[, factor] <- label
#       bootstrapped_data$t1 <- seq(min(subsetted_data$t1), max(subsetted_data$t1), by=resolution)
#       bootstrapped_data$mean <- 0
#       bootstrapped_data$se <- 0
#       bootstrapped_data$CI025 <- 0
#       bootstrapped_data$CI975 <- 0
#       
#       bootstrapped_data <- bootstrapped_data[, c(factor,'t1','mean','se','CI025','CI975',sample_rows)]
#       
#       # se is the SD of the sample
#       bootstrapped_data$se <- apply(bootstrapped_data[, sample_rows], 1, sd)
#       bootstrapped_data$mean <- apply(bootstrapped_data[, sample_rows], 1, mean)
#       bootstrapped_data$CI025 <- bootstrapped_data$mean - 1.96*bootstrapped_data$se
#       bootstrapped_data$CI975 <- bootstrapped_data$mean + 1.96*bootstrapped_data$se
#       #bootstrapped_data$CI025 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.025) })
#       #bootstrapped_data$CI975 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.975) })
#       
#       # using dplyr's rbind_list or speed increase
#       #combined_bootstrapped_data <- rbind(combined_bootstrapped_data,bootstrapped_data)
#       combined_bootstrapped_data <- rbind_list(combined_bootstrapped_data,bootstrapped_data)
#     }
#   }
#   else {
#     # within-subjects:
#     
#     # for within-subjects, we need to calculate the difference between
#     # level 1 and 2 of the factor for each subject before sampling splines
#     library(reshape2)
#     data <- dcast(data, as.formula(paste(paste(data_options$participant_factor,'t1',sep=" + "),factor,sep=' ~ ')), value.var='BinMean', fun.aggregate = mean, drop = TRUE)
#     
#     # re-calculate BinMean as the DIFFERENCE between the two labels
#     data$BinMean <- data[, 3] - data[, 4]
#     
#     # remove all samples where BinMean == NA
#     data <- data[!is.na(data$BinMean), ]
#     
#     data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
#     
#     cat('Sampling')
#     bootstrapped_data <- replicate(samples, spline_sampler(data, data_options, resolution))
#     bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))
#     
#     sample_rows <- paste('Sample', c(1:samples), sep="")
#     
#     # use data.table for speed increase
#     #colnames(bootstrapped_data) <- sample_rows
#     setnames(bootstrapped_data, sample_rows)
#     
#     bootstrapped_data$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
#     bootstrapped_data$mean <- 0
#     bootstrapped_data$se <- 0
#     bootstrapped_data$CI025 <- 0
#     bootstrapped_data$CI975 <- 0
#     
#     bootstrapped_data <- bootstrapped_data[, c('t1','mean','se','CI025','CI975',sample_rows)]
#     
#     # se is the SD of the sample
#     bootstrapped_data$se <- apply(bootstrapped_data[, sample_rows], 1, sd)
#     bootstrapped_data$mean <- apply(bootstrapped_data[, sample_rows], 1, mean)
#     bootstrapped_data$CI025 <- bootstrapped_data$mean - 1.96*bootstrapped_data$se
#     bootstrapped_data$CI975 <- bootstrapped_data$mean + 1.96*bootstrapped_data$se
#     #bootstrapped_data$CI025 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.025) })
#     #bootstrapped_data$CI975 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.975) })
#     
#     # use dplyr's rbind_list for speed increase
#     #combined_bootstrapped_data <- rbind(combined_bootstrapped_data,bootstrapped_data)
#     combined_bootstrapped_data <- rbind_list(combined_bootstrapped_data,bootstrapped_data)
#   }
#   
#   # between-subjects:
#   
#   if (within_subj == FALSE) {
#     bootstrapped_diverged <- combined_bootstrapped_data[, c(factor,'t1','mean','se')]
#     
#     # first, we need to calculate the difference between the two groups at each point
#     library(reshape2)
#     
#     # get mean for each level of factor
#     bootstrapped_diverged_mean <- dcast(bootstrapped_diverged, paste('t1',factor,sep=' ~ '), value.var = 'mean', fun.aggregate = mean, drop = TRUE)
#     
#     # get se for each level of factor
#     bootstrapped_diverged_se <- dcast(bootstrapped_diverged, paste('t1',factor,sep=' ~ '), value.var = 'se', fun.aggregate = mean, drop = TRUE)
#     colnames(bootstrapped_diverged_se) <- c('t1','Group1SE','Group2SE')
#     bootstrapped_diverged <- cbind(bootstrapped_diverged_mean, bootstrapped_diverged_se[, c('Group1SE','Group2SE')])
#     rm(bootstrapped_diverged_se)
#     
#     # calculate SD
#     N <- length(levels(data[, data_options$participant_factor]))
#     
#     bootstrapped_diverged$Group1SD <- bootstrapped_diverged$Group1SE*sqrt(N)
#     bootstrapped_diverged$Group2SD <- bootstrapped_diverged$Group2SE*sqrt(N)
#     
#     bootstrapped_diverged$PooledSDSquared <- (bootstrapped_diverged$Group1SD^2 + bootstrapped_diverged$Group2SD^2) / 2
#     bootstrapped_diverged$PooledSE <- sqrt(bootstrapped_diverged$PooledSDSquared)*sqrt((1/N) + (1/N))
#     
#     bootstrapped_diverged$t <- (abs(bootstrapped_diverged[, 3] - bootstrapped_diverged[, 2])) / bootstrapped_diverged$PooledSE
#     
#     required_t <- abs(qt((alpha/2), (N*2)-2))
#     
#     bootstrapped_diverged$Diverged <- 0
#     bootstrapped_diverged[which(bootstrapped_diverged$t >= required_t), 'Diverged'] <- 1
#   }  
#   else {
#     bootstrapped_diverged <- combined_bootstrapped_data[, c('t1','mean','se')]
#     
#     # within-subjects
#     # just do a comparison to 0 for difference (mean) and standard error
#     
#     bootstrapped_diverged$t <- bootstrapped_diverged$mean / bootstrapped_diverged$se
#     
#     N <- length(levels(data[, data_options$participant_factor]))
#     required_t <- abs(qt((alpha/2), (N*2)-2))
#     
#     bootstrapped_diverged$Diverged <- 0
#     bootstrapped_diverged[which(bootstrapped_diverged$t >= required_t), 'Diverged'] <- 1
#   }
#   
#   # return everything!
#   list(
#     samples = combined_bootstrapped_data,
#     divergence = bootstrapped_diverged
#   )
# }
# 
# # bootstrapped_splines_nonparametric(data, data_options, factor, within_subj, samples, resolution, alpha)
# #
# # Bootstrap splines from a time_analysis() file. Returns a plottable time series dataframe with means for each
# # level of Factor (i.e., each condition), as well as a time series dataframe which shows the points of divergence
# # throughout the time window.
# #
# # PROBLEM: The standard errors returned by this function are questionable because they assume
# # that each participant is included at each time point. In some sense, they are (after the
# # smoothing spline is applied), but it makes spurious VISIBLE divergences at the beginnings/ends of trials
# # (when few participants are looking) much more likely. Importantly, the bootstrapping
# # procedure means that these apparent divergences are not actually significant, but it's a bit
# # confusing because visually they look significant.
# #
# # @param dataframe data Your clean dataset
# # @param list data_options Standard list of options for manipulating dataset
# # @param string factor What factor to split by? Maximum two conditions!
# # @param boolean within_subj Are the two conditions within or between subjects?
# # @param int samples How many (re)samples to take?
# # @param float resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100 intervals per second, or hundredths of a second
# # @param float alpha p-value when the groups are sufficiently "diverged"
# #
# # @return list(samples, divergence)
# bootstrapped_splines_nonparametric <- function (data, data_options, factor = '', within_subj = FALSE, samples=1000, resolution=10, alpha = .05) {
#   # dependencies
#   library(plyr, quietly=TRUE)
#   #library(data.table, quietly=TRUE)
#   
#   # define spline function
#   fit_splines <- function (data) {
#     splines_set <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(length(unique(data$ParticipantName)) + 3)))
#     colnames(splines_set) <- c('t1','Mean','SE',as.vector(unique(data$ParticipantName)))
#     splines_set$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
#     
#     for (participant in unique(data$ParticipantName)) {
#       spline_model <- with(subset(data, ParticipantName == participant), smooth.spline(t1, BinMean, cv=FALSE))
#       
#       # get interpolated spline predictions for total time at desired resolution
#       splines_set[, participant] <- predict(spline_model, splines_set$t1)$y
#     }
#     
#     # we now have splines with the spline model for each participant
#     # calculate Mean and SE (i.e., SD/sqrt(N)) at each timepoint
#     
#     splines_set$Mean <- apply(splines_set[, as.vector(unique(data$ParticipantName))], 1, function (x) { mean(x) })
#     splines_set$SE <- apply(splines_set[, as.vector(unique(data$ParticipantName))], 1, function (x) { sd(x) / sqrt(length(x)) })
#     
#     splines_set
#   }
#   
#   # convert resolution to seconds
#   resolution = resolution / 1000;
#   
#   # validate variables
#   if (factor == '') {
#     error('bootstrapped_splines','No factor specified. Must specify a factor with 2 levels.')
#   }
#   
#   if (length(levels(data[, factor])) != 2) {
#     error('bootstrapped_splines','Factor must have 2 levels.')
#   }
#   
#   # re-factor Participant name column, so that levels() is accurate
#   data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
#   
#   # between-subjects:
#   if (within_subj == FALSE) {
#     splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(length(unique(data$ParticipantName)) + 6)))
#     colnames(splines) <- c('t1','Mean1','SE1','Mean2','SE2','MeanDifference',as.vector(unique(data$ParticipantName)))
#     splines$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
#     
#     label_number <- 1
#     for (label in levels(data[, factor])) {
#       subsetted_data <- data[which(data[, factor] == label), ]
#       
#       # fit splines for each participant
#       spline_fits <- fit_splines(subsetted_data)
#       
#       # add to main splines dataframe
#       splines[, c(paste('Mean',label_number,sep=''), paste('SE',label_number,sep=''), as.vector(unique(subsetted_data$ParticipantName)))] <- spline_fits[, c('Mean','SE',as.vector(unique(subsetted_data$ParticipantName)))]
#       
#       # increment label_number
#       label_number <- label_number + 1
#     }
#     
#     # calculate MeanDifference at each point
#     splines$MeanDifference <- splines$Mean2 - splines$Mean1
#     
#     # we now have our spline fit for the real data
#     # let's random shuffle the condition labels and see how likely these divergences are
#     # by chance
#     
#     # this dataframe will hold each of the mean differences between conditions calculated
#     # at each timepoint after shuffling condition labels (i.e., by random chance)
#     bootstrapped_splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(samples + 4)))
#     colnames(bootstrapped_splines) <- c('t1','RealDifference','p_value','Diverged',paste('Sample',c(1:samples),sep=''))
#     bootstrapped_splines$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
#     bootstrapped_splines$RealDifference <- splines$MeanDifference
#     
#     for (i in 1:samples) {
#       # shuffle conditions
#       participants_assignments <- ddply(data, c('ParticipantName',factor), summarize, Mean = mean(BinMean))
#       participants_assignments[, factor] <- sample(participants_assignments[, factor], length(participants_assignments[, factor]))
#       
#       for (participant in as.vector(unique(data$ParticipantName))) {
#           new_condition <- participants_assignments[which(participants_assignments$ParticipantName == participant), factor]
#           data[which(data$ParticipantName == participant), factor] <- new_condition
#       }
#       
#       shuffled_splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(length(unique(data$ParticipantName)) + 6)))
#       colnames(shuffled_splines) <- c('t1','Mean1','SE1','Mean2','SE2','MeanDifference',as.vector(unique(data$ParticipantName)))
#       
#       label_number <- 1
#       for (label in levels(data[, factor])) {
#         subsetted_data <- data[which(data[, factor] == label), ]
#         
#         # fit splines for each participant
#         spline_fits <- fit_splines(subsetted_data)
#         
#         # add to main splines dataframe
#         shuffled_splines[, c(paste('Mean',label_number,sep=''), paste('SE',label_number,sep=''), as.vector(unique(subsetted_data$ParticipantName)))] <- spline_fits[, c('Mean','SE',as.vector(unique(subsetted_data$ParticipantName)))]
#         
#         # increment label_number
#         label_number <- label_number + 1
#       }
#       
#       shuffled_splines$MeanDifference <- shuffled_splines$Mean2 - shuffled_splines$Mean1
#       
#       # now add to bootstrapped_splines where we keep track of these random differences
#       bootstrapped_splines[, paste('Sample',i,sep='')] <- shuffled_splines$MeanDifference
#     }
#     
#     # now how many times did a divergence this big happen by random chance with shuffled labels?
#     bootstrapped_splines$p_value <- apply(bootstrapped_splines[, c('RealDifference',paste('Sample',c(1:samples),sep=''))], 1, function(x) { length(x[which(abs(x) >= abs(x[1]))]) / length(x) }) # note this function includes an inherent +1 because it includes the RealDifference itself
#     bootstrapped_splines$Diverged <- ifelse(bootstrapped_splines$p_value < alpha, 1, 0)
#   }
#   else {
#     # within-subjects:
#     
#     # for within-subjects, we need to calculate the difference between
#     # level 1 and 2 of the factor for each subject before sampling splines
#     library(reshape2)
#     data <- dcast(data, as.formula(paste(paste(data_options$participant_factor,'t1',sep=" + "),factor,sep=' ~ ')), value.var='BinMean', fun.aggregate = mean, drop = TRUE)
#     
#     # re-calculate BinMean as the DIFFERENCE between the two labels
#     data$BinMean <- data[, 3] - data[, 4]
#     
#     # remove all samples where BinMean == NA
#     data <- data[!is.na(data$BinMean), ]
#     
#     data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
#     
#     # fit splines for each participant
#     splines <- fit_splines(data)
#     
#     # we now have our spline fit for the real data
#     # let's random shuffle the condition labels and see how likely these divergences are
#     # by chance
#     
#     # this dataframe will hold each of the mean differences between conditions calculated
#     # at each timepoint after shuffling condition labels (i.e., by random chance)
#     bootstrapped_splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(samples + 4)))
#     colnames(bootstrapped_splines) <- c('t1','RealDifference','p_value','Diverged',paste('Sample',c(1:samples),sep=''))
#     bootstrapped_splines$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
#     bootstrapped_splines$RealDifference <- splines$Mean
#     
#     for (i in 1:samples) {
#       # shuffle conditions
#       for (participant in as.vector(unique(data$ParticipantName))) {
#         if (rbinom(1,1,.5) == 1) {
#           # shuffle!
#           # all we actually have to do "shuffle" this data is flip the sign on the BinMean for this
#           # participant, because it's already cast down to a difference score
#           
#           data[which(data$ParticipantName == participant), 'BinMean'] <- -data[which(data$ParticipantName == participant), 'BinMean']
#         }
#         else {
#           # no shuffling here...
#         }
#       }
#       
#       spline_fits <- fit_splines(data)
#       
#       # now add to bootstrapped_splines where we keep track of these random differences
#       bootstrapped_splines[, paste('Sample',i,sep='')] <- spline_fits$Mean
#     }
#     
#     # now how many times did a divergence this big happen by random chance with shuffled labels?
#     bootstrapped_splines$p_value <- apply(bootstrapped_splines[, c('RealDifference',paste('Sample',c(1:samples),sep=''))], 1, function(x) { length(x[which(abs(x) >= abs(x[1]))]) / length(x) }) # note this function includes an inherent +1 because it includes the RealDifference itself
#     bootstrapped_splines$Diverged <- ifelse(bootstrapped_splines$p_value < alpha, 1, 0)
#   }
#   
#   # return everything!
#   list(
#     bootstrapped_splines = bootstrapped_splines,
#     splines = splines
#   )
# }
# 
# # get_looks(data, data_options, smoothing, factors)
# #
# # Find all looks to AOIs in a dataset and describe them. This function fills in 1-sample gaps
# # caused by trackloss between sequential gazes to the same location.
# #
# # @param dataframe data
# # @param list data_options
# # @param boolean smoothing
# # @param character.vector factors
# #
# # @return dataframe looks
# get_looks <- function (data, data_options, smoothing = 1, factors = NA) {
#   # set defaults if necessary
#   if (length(factors) < 2 && is.na(factors)) {
#     factors <- data_options$default_factors
#   }
#   
#   # make sure all outside looks are treated as trackloss
#   data <- treat_outside_looks_as_trackloss(data, data_options)
#   
#   # first, make sure all trackloss results in an NA within the AOI column
#   data[which(data[, data_options$trackloss_factor] == 1), data_options$active_aoi_factor] <- NA
#              
#   # for each AOI within each trial within each subject,
#   # do some smoothing to get rid of trackloss issues
#   
#   looks <- data.frame(matrix(nrow = 0, ncol = 11 + length(factors)))
#   
#   # we'll store this in column_names so we can keep using it
#   column_names <- c(data_options$participant_factor, factors, data_options$trial_factor, data_options$active_aoi_factor, 'SampleLength', 'StartSample', 'EndSample', 'TimeLength', 'StartTime', 'EndTime', 'RankLength', 'RankSequence')
#   
#   colnames(looks) <- column_names
#   
#   for (participant in unique(data[, data_options$participant_factor])) {
#     for (factor in factors) {
#       for (factor_level in unique(data[, factor])) {
#         for (trial in unique(data[, data_options$trial_factor])) {
#           # get matching rows for this operation
#           trial_rows <- which(data[, data_options$participant_factor] == participant & data[, factor] == factor_level & data[, data_options$trial_factor] == trial);
#           
#           # calculate AOI runs within these trial rows
#           # then look for any chances for smoothing
#           runs <- rle(as.character(data[trial_rows, data_options$active_aoi_factor]))
#           
#           for (i in 1:length(runs$values)) {
#             if (length(runs$values[i]) > 1 && is.na(runs$values[i]) && i > 1) {
#               # if the NA look is <= our smoothing sample value, let's fill it in with
#               # the last value
#               if (runs$lengths[i] <= smoothing && !is.na(runs$values[i - 1])) {
#                 # smooth it!
#                 # kill this value by setting its length to 0
#                 runs$lengths[i] <- 0
#                 
#                 # then add a length to the preceding value
#                 runs$lengths[i - 1] <- runs$length[i - 1] + 1
#               }
#             }
#           }
#           
#           # regenerate runs with the smoothed dataset
#           runs <- rle(inverse.rle(runs))
#           
#           if (length(runs$values) > 0) {          
#             # now add the final runs to the looks dataframe
#             looks_to_bind <- data.frame(matrix(nrow = length(runs$values), ncol = 11 + length(factors)))
#             colnames(looks_to_bind) <- column_names
#             
#             looks_to_bind[, data_options$participant_factor] <- participant
#             
#             for (factor in factors) {
#               looks_to_bind[, factor] <- as.character(data[trial_rows[1], factor])
#             }
#             
#             looks_to_bind[, data_options$trial_factor] <- trial
#             looks_to_bind[, data_options$active_aoi_factor] <- runs$values
#             looks_to_bind$SampleLength <- runs$lengths
#             looks_to_bind$StartSample <- c(1,cumsum(runs$lengths)[1:length(runs$lengths) - 1] + 1)
#             looks_to_bind$EndSample <- cumsum(runs$lengths) + 1
#             looks_to_bind$TimeLength <- looks_to_bind$SampleLength * (1000/data_options$sample_rate)
#             looks_to_bind$StartTime <- (looks_to_bind$StartSample - 1) * (1000/data_options$sample_rate)
#             looks_to_bind$EndTime <- (looks_to_bind$EndSample - 1) * (1000/data_options$sample_rate)
#             looks_to_bind$RankLength <- rank(-looks_to_bind$SampleLength)
#             looks_to_bind$RankSequence <- 1:length(runs$lengths)
#             
#             # bind!
#             looks <- rbind(looks, looks_to_bind)
#           }
#         }
#       }
#     }
#   }
#   
#   looks
# }
# 
# 
# # subset_by_window ()
# #
# # Return a subset of the dataframe by the window parameters
# #
# # @param dataframe data
# # @param list data_options
# # @param integer window_start
# # @param integer window_end
# #
# # @return dataframe data
# subset_by_window <- function (data, data_options, window_start = NA, window_end = NA) {
#   # do we have a looking window?  if so, filter by one or both of the window parameters
#   if (!is.na(window_start) & is.na(window_end)) {
#     message('subset_by_window','Trimming data before window...')
#     data <- data[which(data[, data_options$time_factor] >= window_start), ]
#   }
#   else if (is.na(window_start) & !is.na(window_end)) {
#     message('subset_by_window','Trimming data after window window...')
#     data <- data[which(data[, data_options$time_factor] <= window_end), ]
#   }
#   else if (!is.na(window_start) & !is.na(window_end)) {
#     message('subset_by_window','Trimming data outside of window...')
#     data <- data[which(data[, data_options$time_factor] >= window_start & data[, data_options$time_factor] <= window_end), ]
#   }
#   
#   data
# }
# 
# # bin_data ()
# # 
# # This function will bin values in one column, "dv", grouping by any number of "factors"
# # and one "sample_factor" column. The sample_factor column is divided by bin_samples and rounded down to bin.
# # All rows that have the same values for the new binned sample column and factors are grouped
# # together. Within each group, the mean, length, and sum are calculated on the dv.
# #
# # @param dataframe data
# # @param list data_options
# # @param integer bin_samples How many samples to put in a single bin?
# # @param character.vector factors A vector of factor columns to aggregate by
# # @param string dv The column to act as the DV
# #
# # @return dataframe binned_data
# bin_data <- function(data, data_options, bin_samples, factors, dv) {  
#   library(data.table, quietly=TRUE)
#   
#   sample_factor <- data_options$sample_factor
#   
#   # set all rows of the var that are NA to 0, so we don't get errors in calculating a mean
#   # if we have rows like this, we have to assume they meant to keep trackloss, even though
#   # keep_trackloss() would have already set NA to 0...
#   data[is.na(data[, dv]), dv] <- 0
#   
#   # after we subset the data and our "zero" point is actually, say, 8000 samples from the 
#   # phase onset, we run into trouble if we ask it to bin by a number of frames
#   # that does not go into the minimum value evenly.  So, let's re-calibrate our sample_factor column
#   # with a nice clean zero point.
#   data[, sample_factor] <- as.numeric(as.character(data[, sample_factor]))
#   
#   if (min(data[,sample_factor]) > 0) {
#     data[,sample_factor] <- data[,sample_factor] - min(data[,sample_factor])
#   }
#     
#   # If binsize=3, then 0,1,2 will go to 0; 3,4,5 will go to 1, and so on. 
#   message('bin_data', sprintf("Dividing values in column %s by bin size %d and rounding down... \n", dv, bin_samples))        
#   data[,sample_factor] <- floor(data[,sample_factor] / bin_samples)
#   
#   # Collapse all rows where constants+timevar columns are the same 
#   message('bin_data', sprintf("Processing column %s, in groups where the following columns have the same value:\n", dv))
#   message('bin_data', sprintf("    %s\n", paste(c(factors, sample_factor), collapse=", ")))
#   message('bin_data', " mean... ")
#   data.mean   <- aggregate(data[, dv], by=data[c(factors, sample_factor)], 'mean')
#   #names(data.mean)[names(data.mean) == dv]     <- "BinMean"
#   setnames(data.mean, 'x', 'BinMean')
#   
#   # Get the number of rows in each bin
#   message('bin_data', " bin size... ")
#   data.length <- aggregate(data[, dv], by=data[c(factors, sample_factor)], 'length')
#   #names(data.length)[names(data.length) == dv] <- "BinSize"
#   setnames(data.length, 'x', 'BinSize')
#   
#   # Get the sum of the rows
#   message('bin_data', " sum... \n")
#   data.sum    <- aggregate(data[, dv], by=data[c(factors, sample_factor)], 'sum')
#   #names(data.sum)   [names(data.sum)   == dv] <- "BinSum"
#   setnames(data.sum, 'x', 'BinSum')
#   
#   message('bin_data', "Merging data frames for mean, bin size, and sum...\n")
#   
#   #datab <- merge(data.mean, data.length, c(factors, sample_factor))
#   datab <- left_join(data.mean, data.length, by=c(factors, sample_factor))
#   
#   #datab <- merge(datab,     data.sum,    c(factors, sample_factor))
#   datab <- left_join(datab, data.sum, by=c(factors, sample_factor))
#   
#   # Sort by all columns, from left to right
#   datab <- datab[ do.call(order, as.list(datab)), ]
#   
#   datab
# }
# 
# # plot_data()
# #
# # Plot a dv in the data by levels of a factor.
# #
# # @param dataframe data
# # @param list data_options
# # @param string output_file (default: 'graph.png')
# # @param string dv
# # @param string factor
# # @param string title
# # @param string y_title
# # @param string x_title
# # @param string type ('empirical' or 'smoothed')
# # @param vector vertical_lines A vector of timepoints to add a vertical black line
# # @param integer bin_time Time (ms) to bin data by in plot
# # @param integer x_gap Plot the time on the x-axis incrementing by this much
# # @param integer width Width of plot in pixels
# # @param integer height Height of plot in pixels
# #
# # @return null Saves file to workingdirectory/output_file
# plot_data <- function(data, data_options, output_file = 'graph.png', dv = NA, factor = 'Condition', title = 'Looking', y_title = 'Proportion', x_title = 'Time (ms)', type = 'empirical', vertical_lines = c(), bin_time = 100, x_gap = 500, width = 1000, height = 600) {
#   # require dependencies
#   require('ggplot2', quietly = TRUE)
#   require('ggthemes', quietly = TRUE)
#   require('reshape2', quietly = TRUE)
#   
#   # use defaults if unset
#   if (is.na(dv)) {
#     dv = data_options$default_dv
#   }
#   
#   # zero the time align
#   data$TimeAlign <- data[, data_options$time_factor] - min(data[, data_options$time_factor])
#   
#   # aggregate by participants, factor, and time
#   data <- aggregate(data.frame(data[, dv]), by = list(data[, data_options$participant_factor], data[, factor], data$TimeAlign), FUN = mean, na.rm = TRUE)  
# 
#   # rename columns
#   # call the participant column 'ParticipantName' because it will be called in spaghetti()
#   # and its easier to just have it with this name
#   colnames(data) <- c('ParticipantName',factor,'TimeAlign',dv)
#   
#   # make sure the factor is a factor
#   data[, factor] <- factor(data[, factor])
#   
#   # now pass off to the spaghetti function...
#   spaghetti(
#       data,
#       fname=output_file,
#       onset=0,
#       addOnsetLine=FALSE,
#       plotwidth=width,
#       plotheight=height,
#       meltids=c(data_options$participant_factor,"TimePlot",factor),
#       meltfactors=c(factor),
#       timeAlignVar="TimeAlign",
#       colorvariable=factor,
#       errbarvars=NA,
#       srate=(1000/data_options$sample_rate),
#       region=c(dv),
#       sample=bin_time,
#       downsample="bin",
#       vertical_lines = vertical_lines,
#       upperlimit=max(data$TimeAlign),
#       xbreakstep=x_gap,
#       plottype=type,
#       graphTitle=title,
#       y_title = y_title,
#       x_title = x_title,
#       showplot=TRUE
#     )
# }
# 
# # spaghetti()
# #
# # Plot eyetracking data in a spaghetti plot
# #
# spaghetti = function(
#   # version 0.1
#   # written by jdegen@bcs.rochester.edu
#   # 01/11/2012
#   
#   # modified by Brock Ferguson
#   #   * set default options for this library
#   #   * made default formatting better
#   #   * added better formatting options, like titles
#   #   * aggregated across subjects within a given bin so that they don't contribute
#   #     multiple datapoints and lower the standard error
#   
#   # assumes that there are no rows where the value in the column coding region that is currently being looked at is empty. ie for ExAnalysis data.frames: rp_RegionType != ""
#   data,
#   meltids=c("TimeFromMovieOnset"), # vector of column names (as strings) to melt the dataset by (i.e. that you want to have the option of plotting by)
#   meltfactors, # the variables in meltids that are factors. Necessary for as.factor(as.character) calls  	
#   region=c("Target"), # vector of regions - region names must be column names in data
#   colorvariable=NA, # variable you want to assign different colors by
#   colormapping=NA, # a data.frame with two factor columns. One column with name of variable you want to map different colors to. One called "Color" with the color for each factor level. Data.frame row names must be levels of mapping variable, and data.frame must be sorted by mapping variable. 
#   linevariable=NA, # variable (column name in data), levels of which you want to plot in different line types. Currently support only for two levels.
#   sizevariable=NA, # variable (column name in data), levels of which you want to plot in different sizes. you can't specify a sizevariable if you haven't specified a linevariable. Currently support only for two levels.
#   facetvariable=NA, # variable (column name in data), levels of which you want to plot in different grids. uses facet_wrap.
#   errbars=TRUE, # plot error bars by default	
#   errbarvars=NA, # vector of column names (as strings) in the melted dataset, the combination of which error bars are to be calculated from (only relevant if drawing ribbons/error bars)
#   dataid="DataID", # name of the variable in the data.frame that uniquely codes samples. downsampling assumes that the data.frame is ordered by this variable, and that sample i and sample i+1 are in fact taken at sampling points t and t+1 (where t is the time step defined by the sampling frequency of the tracker, e.g. 4ms is the default for the EyeLink 500)
#   sample=20, # ms to downsample to
#   downsample="down", # down: downsample. bin: bin data into time bins of size sample
#   srate=4, # sampling rate in ms. 4ms is the default for the EyeLink 500	
#   half="both", # only generalized for half="both". if half = "first" or "second", assumes number of trials in my color_gumballs dataset
#   exclude=NA, # don't exclude data by default. if exclude is list of vectors of character strings, interpret character strings as levels of the variables denoted by the names of the list elements and exclude those from data. e.g. exclude=list(Var1=c("a","b"),Var2=c("1")). only works with factors.
#   onset = 100, # time that 0-point of linguistic event of interest should be aligned to, in ms
#   upperlimit = 5133, # upper limit on x axis (time in ms)
#   timeAlignVar=NA, # column that contains time variable that you want to align by (ie you want to align at the 0 point of that variable)
#   addOnsetLine=TRUE, # add a vertical line at linguistic event onset
#   addVerticalLine=FALSE, # add a second vertical line at eg mean onset of another event. provide name of that time variable where 0 is the onset of that event, e.g. "Time_rel_stim_Adjective"
#   extraVerticalType="collapsed", # one of "individual" or "collapsed". Either prints mean onset of each level of the additional linguistic event of interest (individual) or overall mean (sollapsed).
#   #	onsetReg=c("competitor","target"), # regions that can have been looked to at onset of linguistic event of interest
#   plottype="empirical", # one of "empirical" or "smoothed". The former plots empirical means (without error bars), the latter plots smoothed predicted means based on y~x. If you want to change the formula for the smoother, set form.
#   form=formula("y~x"), # formula to pass to smoother. 
#   method = "auto", # method for smoother to use. can be e.g. lm, glm, loess, etc. see stat_smooth for details
#   #	aggregateby=FALSE, # if downsample == "bin" and aggregateby is a string, proportions will be computed for timewindows of size sample and aggregated by aggregateby (eg "Subject")
#   analysiswindow=FALSE, # don't plot analysis window by default. when TRUE, not yet generalized. maybe add extra argument of c(window_start,window_end) times, relative to onset of linguistic event of interest?
#   analysisonset=0, 
#   extraopts=FALSE, # don't change the text size etc defaults. when TRUE, sets legend text size to 20, axis text size to 15, axis title size to 20, legend background to white, and panel label size to 20
#   #	align="qonset", # figure out if you can generalize this at all
#   xbreakstep=200, # size of tick mark spacing on x-axis
#   plotwidth=800, # width of the plot in pixels
#   plotheight=450, # height of the plot in pixels
#   fname=NA, # file name (quoted). if left NA, generates a filename for pdf that is not generalized	
#   showplot=FALSE, # don't print plot by default (save straight to file instead). If TRUE, will both print plot in quartz window and print plot to file
#   
#   # parameters deleted by Brock
#   # graphdir="./", # path to directory where graph will be saved
#   
#   # parameters added by Brock
#   graphTitle = "Looking Proportions",
#   y_title = 'Proportion Looking',
#   x_title = 'Time (ms)',
#   timeAdjust = 0,
#   vertical_lines = NA, # e.g., c(15000,20000)
#   ...
# )
# {
#   # remove data based on specified factor levels	
#   if (!is.na(exclude))
#   {
#     for (e in names(exclude))
#     {
#       data = data[!data[,e] %in% exclude[[e]],]
#       print(paste("removed levels:",exclude[[e]],"of variable",e))
#     }
#     print(paste("after removing factor levels:",nrow(data)))
#   }
#   
#   # remove first/second half
#   if (half == "first")
#   {
#     data <- subset(data, TrialNumber <= 120/2)
#     print(paste("after removing second half:",nrow(data)))
#   }
#   if (half == "second")
#   {
#     data <- subset(data, TrialNumber > 120/2)
#     print(paste("after removing first half:",nrow(data)))		
#   }		
#   
#   # create time variable to plot on x axis and remove samples not in desired time window
#   print(paste("event onset at", onset))
#   data$TimePlot = data[,timeAlignVar] + onset
#   
#   if (addVerticalLine != FALSE) { data$NewAdj = onset + (data[,timeAlignVar] - data[,addVerticalLine]) }
#   data = subset(data, TimePlot >= 0 & TimePlot <= upperlimit)
#   print(paste("after removing samples before sentence onset and after upper limit:",nrow(data)))
#   
#   if (downsample == "bin")
#   {
#     print(paste("binning into time bins of",sample,"ms"))
#     data$TimePlot <- (data$TimePlot %/% sample) * sample
#     print(paste("after binning data:",nrow(data)))		
#   } else {
#     if (downsample == "down")	
#     {
#       print(paste("downsampling to",sample,"ms"))	
#       data <- data[as.numeric(as.character(data[,"TimePlot"])) %% (sample/srate) == 0,]
#       print(paste("after downsampling:",nrow(data)))			
#     }
#   }
#   
#   if (is.na(fname)) 
#   { 
#     if (fileformat == "png")
#     {
#       fname = "spaghettiplot.png" 
#     } else {
#       if (fileformat == "pdf")	
#       {
#         fname = "spaghettiplot.pdf"
#       }
#     }	
#   }
#   
#   # create a reduced dataset containing only the data you need
#   for (me in meltfactors)
#   {
#     data[,me] = as.factor(as.character(data[,me]))
#   }
#   melted = melt(data,id = meltids,measure=region)
#   melted$value = as.numeric(as.character(melted$value))
#   
#   i=0
#   texty=c()
#   
#   if (is.na(errbarvars))
#   {
#     errbarvars = c(colorvariable,linevariable,sizevariable,facetvariable)
#     errbarvars = errbarvars[!is.na(errbarvars)]
#   }
#   
#   for (v in errbarvars)
#   {
#     if (i==0)
#     {
#       texty=paste(texty,paste("data[,\"",v,"\"]",sep=""),sep="")
#       i=1
#     } else {
#       texty=paste(texty,paste("data[,\"",v,"\"]",sep=""),sep=",")			
#     }
#   }
#   texty=paste("paste(",texty,")",sep="")
#   melted$errbargroup = eval(parse(text=texty))
#   melted$errbargroup = as.factor(as.character(melted$errbargroup))
#   
#   # aggregate over subjects so they don't contribute multiple points to a given bin
#   melted <- with(melted,aggregate(value, by = list(ParticipantName,TimePlot,eval(parse(text=colorvariable)),variable,errbargroup), FUN = mean, na.rm = TRUE))
#   colnames(melted) <- c('ParticipantName','TimePlot',colorvariable,'variable','errbargroup','value')
#   
#   # create the base plot
#   if (is.na(facetvariable))
#   {
#     if (plottype == "empirical")
#     {
#       if (is.na(linevariable))
#       {
#         agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup),FUN="mean",na.rm = TRUE))
#         colnames(agr) = c("TimePlot",colorvariable,"errbargroup","value")		
#         
#         agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup),FUN="se"))$x
#         agr$YMin = agr$value - agr$SE
#         agr$YMax = agr$value + agr$SE
#         limits = aes(ymin=YMin,ymax=YMax)					
#         p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable))
#       } else {			
#         if (is.na(sizevariable))
#         {
#           agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup),FUN="mean",na.rm = TRUE))
#           colnames(agr) = c("TimePlot",colorvariable,linevariable,"errbargroup","value")
#           
#           agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup),FUN="se"))$x	
#           agr$YMin = agr$value - agr$SE
#           agr$YMax = agr$value + agr$SE
#           limits = aes(ymin=YMin,ymax=YMax)		
#           p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable))
#         } else {
#           agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup),FUN="mean",na.rm = TRUE))
#           colnames(agr) = c("TimePlot",colorvariable,linevariable,sizevariable,"errbargroup","value")
#           
#           agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup),FUN="se"))$x
#           agr$YMin = agr$value - agr$SE
#           agr$YMax = agr$value + agr$SE
#           limits = aes(ymin=YMin,ymax=YMax)						
#           
#           p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable)) +
#             scale_size_manual(values=c(3,1.5))	
#         }
#       }
#       p = p +
#         geom_line(aes_string(group="errbargroup"),size=I(2))
#       if (errbars)
#       {
#         p = p + geom_errorbar(limits, size = .4, width = 50)	
#       }
#       
#     } else
#     {
#       if (is.na(linevariable))
#       {
#         p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable))
#       } else {
#         if (is.na(sizevariable))
#         {
#           p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable))			
#         } else {
#           p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable))						
#         }
#       }
#       p = p +
#         stat_smooth(aes_string(group="errbargroup",fill=colorvariable),size=I(1.5),method=method,formula=form)
#       if (!is.na(colormapping)) {
#         p = p + scale_fill_manual(values=colormapping[sort(as.character(unique(melted[,colorvariable]))),]$Color)
#       }				
#     }
#   } else {
#     if (plottype == "empirical")
#     {
#       if (is.na(linevariable))
#       {
#         agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup,eval(parse(text=facetvariable))),FUN="mean",na.rm = TRUE))
#         colnames(agr) = c("TimePlot",colorvariable,"errbargroup",facetvariable,"value")		
#         agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup,eval(parse(text=facetvariable))),FUN="se"))$x
#         agr$YMin = agr$value - agr$SE
#         agr$YMax = agr$value + agr$SE
#         limits = aes(ymin=YMin,ymax=YMax)					
#         p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable)) #+
#       } else {			
#         if (is.na(sizevariable))
#         {
#           agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="mean",na.rm = TRUE))
#           colnames(agr) = c("TimePlot",colorvariable,linevariable,"errbargroup",facetvariable,"value")
#           agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="se"))$x	
#           agr$YMin = agr$value - agr$SE
#           agr$YMax = agr$value + agr$SE
#           limits = aes(ymin=YMin,ymax=YMax)		
#           p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable)) #+
#         } else {
#           agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="mean",na.rm = TRUE))
#           colnames(agr) = c("TimePlot",colorvariable,linevariable,sizevariable,"errbargroup",facetvariable,"value")
#           agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="se"))$x
#           agr$YMin = agr$value - agr$SE
#           agr$YMax = agr$value + agr$SE
#           limits = aes(ymin=YMin,ymax=YMax)						
#           
#           p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable)) +
#             scale_size_manual(values=c(3,1.5))	
#         }
#       }
#       p = p +
#         geom_line(aes(group=errbargroup),size=I(1))
#       if (errbars)
#       {
#         p = p + geom_errorbar(limits,linetype=I(1))	
#       }
#       
#     } else
#     {
#       if (is.na(linevariable))
#       {
#         p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable))
#       } else {
#         if (is.na(sizevariable))
#         {
#           p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable))			
#         } else {
#           p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable))						
#         }
#       }
#       p = p +
#         stat_smooth(aes_string(group="errbargroup",fill=colorvariable),size=I(1.5))
#       if (!is.na(colormapping)) {
#         p = p + scale_fill_manual(values=colormapping[sort(as.character(unique(melted[,colorvariable]))),]$Color)			
#       }	
#     }
#     form = as.formula(paste("~",facetvariable,sep=""))
#     p = p +
#       facet_wrap(form)			
#   }
#   
#   # extra stuff that's common to all plots
#   if (analysiswindow) {
#     onset = onset + analysisonset
#   }
#   
#   p <- p +
#        theme_bw(base_family = "Arial", base_size = 16) +
#        scale_colour_hue(l = 60, c = 150, h.start = 0) +
#        theme(legend.position = c(.86,.12)) +
#        theme(axis.title.x = element_text(vjust=-0.3)) + # move x-axis label lower
#        theme(axis.title.y = element_text(vjust=1.15)) + # move y-axis label left
#        theme(panel.grid.minor = element_blank()) + # no grids
#        theme(panel.grid.major = element_blank()) + # no borders
#        theme(panel.border = element_rect(size=1, color='black')) +
#        coord_cartesian(ylim=c(0,1)) +  
#        scale_y_continuous(y_title, breaks = c(0, .25, .5, .75, 1)) +
#        scale_x_continuous(x_title,breaks=seq(0 + timeAdjust,upperlimit + timeAdjust,by=xbreakstep)) 
#   
#   if (addOnsetLine)
#   {
#     p <- p + geom_vline(xintercept=onset, colour="black",size=I(1.5),legend=FALSE)
#   }
#   if (!is.na(colormapping))	
#   {
#     p = p +	
#       scale_colour_manual(values=colormapping[sort(as.character(unique(melted[,colorvariable]))),]$Color)
#   }
#   
#   if (addVerticalLine != FALSE)
#   {
#     means <- with(data,aggregate(NewAdj, by=list(eval(parse(text=colorvariable))), FUN=mean,na.rm = TRUE))
#     row.names(means) <- means$Group.1
#     means <- means[order(means$Group.1),]
#     
#     if (extraVerticalType == "individual")
#     {
#       for (i in 1:length(means$x))
#       {
#         xi <- means$x[i]
#         if (analysiswindow) { xi <- xi + analysisonset }
#         if (!is.na(colormapping))
#         {
#           co <- as.character(colormapping[as.character(means$Group.1[i]),]$Color)
#         } else {
#           co <- "black"
#         }
#         p <- p + geom_vline(xintercept=xi, colour=I(co),size=I(1.5),legend=FALSE)
#       }
#     } else {
#       if (extraVerticalType == "collapsed")	
#       {
#         xi <- mean(means$x)				
#         if (analysiswindow) { xi <- xi + analysisonset }
#         p <- p + geom_vline(xintercept=xi, colour="black", size=I(1.5),legend=FALSE)
#       }
#     }
#   }
#   
#   
#   # set options
#   # p <- p +
#   #  opts(
#   #    legend.text = theme_text(size = 12),
#   #    axis.text.x = theme_text(size=12),
#   #    axis.text.y = theme_text(size=12,hjust=1),
#   #    axis.title.x = theme_text(size=14,vjust=.05),
#   #    axis.title.y = theme_text(size=14,angle=90,vjust=0.25),
#   #    legend.background=theme_rect(fill="white"),
#   #    strip.text.x = theme_text(size=14),
#   #    title = graphTitle,
#   #    plot.title = theme_text(size=18, lineheight=.8, face="bold", vjust=1.5)
#   #  ) 
#   
#   if (length(vertical_lines) > 0 && !is.na(vertical_lines)) {
#     for (i in 1:length(vertical_lines)) {
#       line <- vertical_lines[i]
#       
#       cat("line: ")
#       cat(line)
#       
#       p = p + geom_vline(xintercept=line, colour="black",size=I(1.5))
#     }
#   }
#   
#   if (showplot) { print(p) }
#   # print plot to file	
#   print(paste("printing to",fname))	
#   
#   png(filename = fname,
#          width = plotwidth, height = plotheight,
#          units = "px",
#          res = 400)
#   
#   print(p)
#   dev.off()
#   
#   rm(p)
#   gc() 
# }
# 
# se <- function(x) {
#   y <- x[!is.na(x)] # remove the missing values, if any
#   sqrt(var(as.vector(y))/length(y))
# }
# 
# error <- function(method, message) {
#   stop(paste("(",method,"): ",message,"\n",sep=""), call. = FALSE)
# }
# 
# message <- function(method, message) {
#   cat(paste(message,"\n",sep=""))
# }