# -----------------------------------------------
#
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
#
# @author Brock Ferguson
#         brock.ferguson@gmail.com
#         brockferguson.com
# @created July 24, 2012
#
# -----------------------------------------------

# load dependent libraries
require('psych', quietly = TRUE)

# set_data_options
#
# Create a list of data options which is passed to most of these
# methods.
#
# @param string participant_factor Set to the subject code of the participant
# @param string active_aoi_factor Set to the name of the AOI being gazed at for this sample
# @param string trackloss_factor Set to 1 or 0 depending on whether the sample is lost to trackloss
# @param string time_factor The factor to use for judgments of time (ms)
# @param string trial_factor The unique name of the current trial
#
# @return list of configuration options
set_data_options <- function(
                        participant_factor = 'ParticipantName',
                        active_aoi_factor = 'SceneType',
                        trackloss_factor = 'TrackLoss',
                        time_factor = 'TimeFromMovieOnset',
                        trial_factor = 'Trial'
                    ) {
  list(
            'participant_factor' = participant_factor,
            'active_aoi_factor' = active_aoi_factor,
            'trackloss_factor' = trackloss_factor,
            'time_factor' = time_factor,
            'trial_factor' = trial_factor
          )
}

# describe_dataset()
#
# Output a brief summary of a dataset. This is useful before/after trimming
# and cleaning functions, so that the user can make sure nothing disastrous
# has happened.
#
# @param dataframe data
# @param list data_options
#
# @return null
describe_dataset <- function (data, data_options) {
  cat("Dataset Summary -----------------------------\n")
  cat(paste("Total Rows:\t\t",nrow(data),sep=""))
  cat(paste("Participants:\t\t",length(unique(data[, data_config$participant_factor])),sep=""))
  cat(paste("Trackloss Prop.", mean(data[, data_options$trackloss_factor])))
  cat("End Summary ---------------------------------\n")
}

# clean_by_factor()
#
# Clean a dataset by only retaining rows that match one of several values
# on a specified factor. For example, you may have a column of MovieName and
# only want to retain timeperiods where the participant was watching "TestMovie.mov".
# Or, you may have a column Phase and only be interested in the "Baseline" and
# "Test" phases.
#
# @param dataframe data; e.g., master_clean
# @param list data_options, e.g., data_options
# @param string factor; e.g., 'MovieName'
# @param character.vector allowed_levels; e.g., c('Movie1','Movie2')
#
# @return dataframe data
clean_by_factor <- function (data, data_options, factor, allowed_levels) {
  # refactor column to ensure it is indeed a factor
  data[, factor] <- factor(data[, factor])
  
  # subset data by rows that meet the criterion
  data <- subset(data, data[,factor] %in% allowed_levels)

  data
}

# treat_outside_looks_as_trackloss()
#
# Treat looks outside of any AOIs as trackloss.
#
# @param dataframe data
# @param list data_options
#
# @return dataframe data
treat_outside_looks_as_trackloss <- function(data, data_options) {
  # if the active AOI column is empty or NA, set the trackloss column as 1
  data[which(data[, data_options$active_aoi_factor] == ''), data_options$trackloss_factor] <- 1
  data[is.na(data[, data_options$active_aoi_factor]), data_options$trackloss_factor] <- 1

  data
}

# get_trackloss_data()
#
# Describe the type of trackloss in our data or, more specifically, within a subsetted time
# window of our data.
#
# @param dataframe data
# @param list data_options
# @param integer window_start optional
# @param integer window_end optional
#
# @return list(trackloss, trials_committed)
get_trackloss_data <- function(data, data_options, window_start = NA, window_end = NA) {
  # do we have a looking window?  if so, filter by one or both of the window parameters
  if (!is.na(window_start) & is.na(window_end)) {
    message('get_trackloss_data','Trimming data before window...')
    data <- data[which(data[, data_options$time_factor] >= window_start), ]
  }
  else if (is.na(window_start) & !is.na(window_end)) {
    message('get_trackloss_data','Trimming data after window window...')
    data <- data[which(data[, data_options$time_factor] <= window_end), ]
  }
  else if (!is.na(window_start) & !is.na(window_end)) {
    message('get_trackloss_data','Trimming data outside of window...')
    data <- data[which(data[, data_options$time_factor] >= window_start & data[, data_options$time_factor] <= window_end), ]
  }
  
  # count looking frames by trials/babies
  looking <- aggregate(data[, data_options$trackloss_factor], by = list(data[, data_options$participant_factor], data[, data_options$trial_factor]), 'sum', na.rm = TRUE)
  
  # count total datapoints by trials/babies
  total <- aggregate(data[, data_options$participant_factor], by = list(data[, data_options$participant_factor],data[, data_options$trial_factor]), 'length')
  looking$TotalFrames <- total[, 3]
  
  colnames(looking) <- c(data_options$participant_factor, data_options$trial_factor,'SamplesLooking','TotalSamples')
  
  # now flip the SamplesLooking column, as it currently counts the trackloss (not good frames)
  looking$SamplesLooking <- looking$TotalSamples - looking$SamplesLooking
  
  # add new columns for convenience
  looking$TracklossFrames <- looking$TotalFrames - looking$FramesLooking
  looking$TracklossProportion <- looking$TracklossFrames / (looking$TotalFrames)
  
  # count trials by babies to see which were excluded just by not finishing the experiment
  # or not looking AT ALL (prior to trackloss scrubbing)
  looking_no_zeroes <- looking[which(looking$FramesLooking > 0), ]
  trials_committed <- aggregate(looking_no_zeroes[,'Trial'], by = list(looking_no_zeroes$ParticipantName), 'length')
  colnames(trials_committed) <- c('ParticipantName','Trials')
  
  list (
      trackloss = looking,
      trials_committed = trials_committed
    )
}

# clean_by_trackloss()
#
# Clean our master file with a minimum frames criterion
#
clean_by_trackloss <- function(file = 'master-clean.csv', window_start = NA, window_end = NA, minimum_frames = NA) {
  message('clean_by_trackloss','Loading datafile...')
  data <- read.csv(file)
  
  # copy this object as we will want to manipulate it later
  raw_data <- data
  
  # do we have a looking window?  if so, filter by one or both of the window parameters
  if (!is.na(window_start) & is.na(window_end)) {
    message('clean_by_trackloss','Trimming before window...')
    data <- data[which(data$TimeFromMovieOnset >= window_start), ]
  }
  else if (is.na(window_start) & !is.na(window_end)) {
    message('clean_by_trackloss','Trimming after window...')
    data <- data[which(data$TimeFromMovieOnset <= window_end), ]
  }
  else if (!is.na(window_start) & !is.na(window_end)) {
    message('clean_by_trackloss','Trimming outside of window...')
    data <- data[which(data$TimeFromMovieOnset >= window_start & data$TimeFromMovieOnset <= window_end), ]
  }
  
  looking <- aggregate(data['TrackLoss'], by = list(data[, data_options$participant_factor],data$Trial), 'sum', na.rm = TRUE)
  
  # count total datapoints by trials/babies
  total <- aggregate(data['ParticipantName'], by = list(data[, data_options$participant_factor],data$Trial), 'length')
  looking$TotalFrames <- total[, 3]
  
  colnames(looking) <- c('ParticipantName','Trial','FramesLooking','TotalFrames')
  
  # now flip the FramesLooking column, as it currently counts the trackloss (not good frames)
  looking$FramesLooking <- looking$TotalFrames - looking$FramesLooking
  
  # which should be removed?
  message('clean_by_trackloss','Calculating trials to be dropped...')
  trackloss_trials <- looking[which(looking$FramesLooking < minimum_frames), ]
  
  message('clean_by_trackloss',paste('Dropping ',nrow(trackloss_trials),' trials...',sep=""))
  for (i in 1:nrow(trackloss_trials)) {
    row <- trackloss_trials[i, ]
    
    raw_data <- raw_data[-which(raw_data$Trial == as.character(row[, 'Trial']) & raw_data[, data_options$participant_factor] == as.character(row[, 'ParticipantName'])), ]
  }
  
  # write updated file
  write.csv(raw_data, file)
}

# final_trial_counts()
#
# Calculate how many trials were committed to our analysis by each participant
#
final_trial_counts <- function(file = 'master-clean.csv',window_start = NA, window_end = NA) {
  message('final_trial_counts','Loading datafile...')
  data <- read.csv(file)
  
  # do we have a looking window?  if so, filter by one or both of the window parameters
  if (!is.na(window_start) & is.na(window_end)) {
    message('final_trial_counts','Trimming before window...')
    data <- data[which(data$TimeFromMovieOnset >= window_start), ]
  }
  else if (is.na(window_start) & !is.na(window_end)) {
    message('final_trial_counts','Trimming after window...')
    data <- data[which(data$TimeFromMovieOnset <= window_end), ]
  }
  else if (!is.na(window_start) & !is.na(window_end)) {
    message('final_trial_counts','Trimming outside of window...')
    data <- data[which(data$TimeFromMovieOnset >= window_start & data$TimeFromMovieOnset <= window_end), ]
  }
  
  excluding_trackloss <- data[is.na(data$TrackLoss), ]
  
  # count looking frames by trials/babies
  looking <- aggregate(excluding_trackloss['ParticipantName'], by = list(excluding_trackloss$ParticipantName,excluding_trackloss$Trial), 'length')
  colnames(looking) <- c('ParticipantName','Trial','FramesLooking')
  
  # count trials by babies
  trials_committed <- aggregate(looking['Trial'], by = list(looking$ParticipantName), 'length')
  colnames(trials_committed) <- c('ParticipantName','Trials')
  
  trials_committed
}

# keep_trackloss()
#
# Keep individual trackloss points
#
keep_trackloss <- function(file = 'master-clean.csv') {
  message('keep_trackloss','Reading datafile...')
  data = read.csv(file)
  
  message('keep_trackloss','Get column names...')
  scenes <- levels(data$SceneType)
  
  for (i in 1:length(scenes)) {
    scene <- scenes[i]
    
    if (scene != '') {     
      if (length(data[is.na(data[,scene]), scene]) > 0) {
        data[is.na(data[,scene]), scene] <- 0
      }
    }
  }
  
  write.csv(data, file)
}

# remove_trackloss()
#
# Remove individual trackloss points
#
remove_trackloss <- function(file = 'master-clean.csv') {
  message('remove_trackloss','Reading datafile...')
  data = read.csv(file)
  
  message('remove_trackloss','Removing trackloss datapoints...')
  data <- data[-which(data$TrackLoss == 1), ]
  
  write.csv(data, file)
}

# create_test_file()
#
# Create a final analyzable test file
# 
create_test_file <- function(input_file = 'master-test.csv', output_file = 'master-test.csv', window_start = NA, window_end = NA) {
  message('create_test_file','Loading datafile...')
  data <- read.csv(input_file)
  
  # do we have a looking window?  if so, filter by one or both of the window parameters
  if (!is.na(window_start) & is.na(window_end)) {
    message('create_test_file','Trimming before window...')
    data <- data[which(data$TimeFromMovieOnset >= window_start), ]
  }
  else if (is.na(window_start) & !is.na(window_end)) {
    message('create_test_file','Trimming after window...')
    data <- data[which(data$TimeFromMovieOnset <= window_end), ]
  }
  else if (!is.na(window_start) & !is.na(window_end)) {
    message('create_test_file','Trimming outside of window...')
    data <- data[which(data$TimeFromMovieOnset >= window_start & data$TimeFromMovieOnset <= window_end), ]
  }

  write.csv(data, output_file)
}

# describe_dv_by_condition ()
#
# Describe a single column of data between conditions
#
describe_dv_by_condition <- function(data, dv) {
  describe_data(data, dv, c('Condition'))
}

# describe_dv_by_factors()
#
# Describe a single DV by each of our factors
describe_dv_by_factors <- function(data, dv, factors = c()) {
  dv <- as.list(dv)
  
  for (i in 1:length(dv)) {
    describe_data(data, as.character(dv[i]), factors)
  }
}

# describe_data ()
#
# Our main description function, wrapped by other methods
#
describe_data <- function(data, dv, factors = c()) {
  # check that factors exist
  if (length(factors) == 0) {
    error('describe_data','Factors argument missing.  Factors must be passed as c() vector.')
  }
  
  # get column names
  columns <- colnames(data)
  
  describe_factors <- data.frame(matrix(ncol = length(factors), nrow = nrow(data)))
  
  for (i in 1:length(factors)) {
    factor <- factors[i]
    if (!1 %in% match(columns, factor)) {
      error('describe_data',paste(factor, ' factor missing from dataset.  Each factor must have a column in the data object.',sep=""))
    }
    else if (!is.factor(data[, factor])) {
      # factor it
      data[, factor] <- factor(data[, factor])
    }
    
    #add to list
    describe_factors[, i] <- data[, factor]
  }
  
  cat(paste("DV: ",dv,"\n====================================================================================\n",sep=""))
  
  details <- by(data[, dv], describe_factors, describe_column)
  details <- capture.output(print(details, quote = FALSE))
  
  # we captured the print() output above because the normal formatting of our details
  # text is really off and I want it to look nice.  so we capture it and then manipulate
  # it with regex.
  details <- sub('(.*?)matrix\\.(.*?)\\:',' X1:',details)
  details <- sub('\\[1\\]\\s+',"",details)
  details <- sub('(-+)',"\n------------------------------------------------------------------------------------\n",details)
  details <- sub('X([0-9]+)\\: ([[:alnum:]]+)',"X\\1: \\2\n",details)
  details <- sub('\\n\\s+',"\n",details)
  
  cat(details)
  
  cat("\n\n")
}

describe_column <- function(data) {
  output <- paste("[Mean]: ",round(mean(data, na.rm = TRUE),3),"      ",
                  "[SD]:   ",round(sd(data, na.rm = TRUE),3),"      ",
                  "[Var]:  ",round(var(data, na.rm = TRUE),3),"      ",
                  "[Min]:  ",round(min(data, na.rm = TRUE),3),"      ",
                  "[Max]:  ",round(max(data, na.rm = TRUE),3),"      ",sep="")
  
  output
}

# time_analysis()
#
# Create a dataset that is ready for time-based analyses (either GCA or linear)
#
time_analysis <- function (data, dv = 'Animate', factors = c('Condition')) {
  # make sure our columns are factors
  for (i in 1:length(factors)) {
    data[, factors[i]] <- factor(data[, factors[i]])
  }
  
  # bin by participant
  binned <- bin_data(data, 'FramesFromPhaseOnset', 3, c('ParticipantName',factors), dv)
  
  # rename columns
  colnames(binned) <- c('ParticipantName',factors,'Bin','BinMean','N','y')
  
  # because we may have exclude trial time before our window, let's adjust our bin numbers
  # so they start at 0
  min_bin <- min(binned[, 'Bin'])
  
  if (min_bin > 0) {
    binned$Bin <- binned$Bin - min_bin
  }
  
  # create t1
  binned$t1 <- binned$Bin * .05
  
  # rename as bySubj
  bySubj <- binned
  
  # are we fitting an empirical logit manual
  bySubj$elog <- log( (bySubj$y + .5) / (bySubj$N - bySubj$y + .5) )
  bySubj$wts <- 1 / (bySubj$y + .5) + 1 / (bySubj$N - bySubj$y + .5)
  
  # create ArcSin column
  bySubj$ArcSin <- asin(sqrt(bySubj$BinMean))
  
  # create an "aggregate" column so we can uniquely code each instance of our factors
  bySubj$AggID <- 0
  
  last_row <- NA
  
  for (i in 1:nrow(bySubj)) {
    row <- bySubj[i, ]
    
    if (length(last_row) > 1) {
      for (j in 1:length(factors) + 1) {
        if (row[, j] != last_row[, j]) {
          bySubj[i, 'AggID'] <- max(bySubj$AggID) + 1
        }
      }
      
      bySubj[i, 'AggID'] <- max(bySubj$AggID)
    }
    else {
      bySubj[i, 'AggID'] <- 1
    }
    
    last_row <- row
  }
  
  # factor this column
  bySubj$AggID <- factor(bySubj$AggID)
  
  # generate orthogonal polynomial linearized time correlates
  # generate polygonal time correlates
  t <- poly((unique(bySubj$Bin)), 4)
  
  bySubj$ot1 <- 0
  bySubj$ot2 <- 0
  bySubj$ot3 <- 0
  bySubj$ot4 <- 0
  
  for (i in 1:nrow(t)) {
    bySubj[which(bySubj$Bin == (i - 1)),"ot1"] <- t[i, 1]
    bySubj[which(bySubj$Bin == (i - 1)),"ot2"] <- t[i, 2]
    bySubj[which(bySubj$Bin == (i - 1)),"ot3"] <- t[i, 3]
    bySubj[which(bySubj$Bin == (i - 1)),"ot4"] <- t[i, 4]
  }
  
  # create natural polynomials
  bySubj$t1 <- bySubj$Bin * .05
  bySubj$t2 <- bySubj$t1^2
  
  # fit model
  #if (elog_wts == TRUE) {
  #  bySubj.lmer <- lmer(subjects_model, data = bySubj, weights = 1/wts)
  #}
  # else {
  #  bySubj.lmer <- lmer(subjects_model, data = bySubj)
  #}

  # get R-2 for fit
  # bySubj.r2 <- summary(lm(attr(bySubj.lmer, "y") ~ fitted (bySubj.lmer)))$r.squared
  
  # get p-values
  # if (p_values == TRUE) {
  #   bySubj.p_values <- pvals.fnc(bySubj.lmer, addPlot = FALSE)
  # }
  
  # now bin by participant
  binned <- bin_data(data, 'FramesFromPhaseOnset', 3, c('Trial',factors), dv)
  
  # rename columns
  colnames(binned) <- c('Trial',factors,'Bin','BinMean','N','y')
  
  # because we may have exclude trial time before our window, let's adjust our bin numbers
  # so they start at 0
  min_bin <- min(binned[, 'Bin'])
  
  if (min_bin > 0) {
    binned$Bin <- binned$Bin - min_bin
  }
  
  # rename as byItem
  byItem <- binned
  
  # are we fitting an empirical logit manual
  byItem$elog <- log( (byItem$y + .5) / (byItem$N - byItem$y + .5) )
  byItem$wts <- 1 / (byItem$y + .5) + 1 / (byItem$N - byItem$y + .5)
  
  # create ArcSin column
  byItem$ArcSin <- asin(sqrt(byItem$BinMean))
  
  # create an "aggregate" column so we can uniquely code each instance of Condition:Trial
  byItem$AggID <- 0
  
  last_row <- NA
  
  for (i in 1:nrow(byItem)) {
    row <- byItem[i, ]
    
    if (length(last_row) > 1) {
      for (j in 1:length(factors) + 1) {
        if (row[, j] != last_row[, j]) {
          byItem[i, 'AggID'] <- max(byItem$AggID) + 1
        }
      }
      
      byItem[i, 'AggID'] <- max(byItem$AggID)
    }
    else {
      byItem[i, 'AggID'] <- 1
    }
    
    last_row <- row
  }
  
  # factor this column
  byItem$AggID <- factor(byItem$AggID)
  
  # generate orthogonal polynomial linearized time correlates
  # generate polygonal time correlates
  t <- poly((unique(byItem$Bin)), 4)
  
  byItem$ot1 <- 0
  byItem$ot2 <- 0
  byItem$ot3 <- 0
  byItem$ot4 <- 0
  
  for (i in 1:nrow(t)) {
    byItem[which(byItem$Bin == (i - 1)),"ot1"] <- t[i, 1]
    byItem[which(byItem$Bin == (i - 1)),"ot2"] <- t[i, 2]
    byItem[which(byItem$Bin == (i - 1)),"ot3"] <- t[i, 3]
    byItem[which(byItem$Bin == (i - 1)),"ot4"] <- t[i, 4]
  }
  
  # create natural polynomials
  byItem$t1 <- byItem$Bin * .05
  byItem$t2 <- byItem$t1^2
  
  # crossed by items and subjects
  binned <- bin_data(data, 'FramesFromPhaseOnset', 3, c('ParticipantName','Trial',factors), dv)
  
  # rename columns
  colnames(binned) <- c('ParticipantName','Trial',factors,'Bin','BinMean','N','y')
  
  # because we may have exclude trial time before our window, let's adjust our bin numbers
  # so they start at 0
  min_bin <- min(binned[, 'Bin'])
  
  if (min_bin > 0) {
    binned$Bin <- binned$Bin - min_bin
  }
  
  # are we fitting an empirical logit manual
  binned$elog <- log( (binned$y + .5) / (binned$N - binned$y + .5) )
  binned$wts <- 1 / (binned$y + .5) + 1 / (binned$N - binned$y + .5)
  
  # create ArcSin column
  binned$ArcSin <- asin(sqrt(binned$BinMean))
  
  # create an "aggregate" column so we can uniquely code each instance of Condition:Trial
  binned$AggID <- 0
  
  last_row <- NA
  
  for (i in 1:nrow(byItem)) {
    row <- binned[i, ]
    
    if (length(last_row) > 1) {
      for (j in 1:length(factors) + 1) {
        if (row[, j] != last_row[, j]) {
          binned[i, 'AggID'] <- max(binned$AggID) + 1
        }
      }
      
      binned[i, 'AggID'] <- max(binned$AggID)
    }
    else {
      binned[i, 'AggID'] <- 1
    }
    
    last_row <- row
  }
  
  # factor this column
  binned$AggID <- factor(binned$AggID)
  
  # generate orthogonal polynomial linearized time correlates
  # generate polygonal time correlates
  t <- poly((unique(binned$Bin)), 4)
  
  binned$ot1 <- 0
  binned$ot2 <- 0
  binned$ot3 <- 0
  binned$ot4 <- 0
  
  for (i in 1:nrow(t)) {
    binned[which(binned$Bin == (i - 1)),"ot1"] <- t[i, 1]
    binned[which(binned$Bin == (i - 1)),"ot2"] <- t[i, 2]
    binned[which(binned$Bin == (i - 1)),"ot3"] <- t[i, 3]
    binned[which(binned$Bin == (i - 1)),"ot4"] <- t[i, 4]
  }
  
  # create natural polynomials
  binned$t1 <- binned$Bin * .05
  binned$t2 <- binned$t1^2
  
  list (
      'bySubj' = bySubj,
      'byItem' = byItem,
      'crossed' = binned
    )
}

p_values <- function (model) {
  coefs <- data.frame(summary(model)@coefs)
  # make the p-values a bit more readable
  coefs$p <- format.pval(2*(1-pnorm(abs(coefs$t.value))), digits=2, eps=0.0001)
  
  data.frame(coefs[, 0], coefs$p)
}

# window_analysis()
#
# Collapse time across our entire window and do an analyis with subjects and items as random effecst
#
window_analysis <- function(data, dv, factors = c('Condition')) {
  # collapse across trials within subjects
  test_sum <- aggregate(data[dv], by = data[c('ParticipantName','Trial',factors)], FUN = 'sum')
  test_length <- aggregate(data[dv], by = data[c('ParticipantName','Trial',factors)], FUN = 'length')
  
  # transform our collapsed dataset into something pretty
  test <- data.frame(test_sum[c('ParticipantName','Trial',factors)], test_sum[dv], test_length[dv])
  
  colnames(test) <- c('ParticipantName','Trial',factors,'y','N')
  
  # calculate elog, wts
  test$elog <- log( (test$y + .5) / (test$N - test$y + .5) )
  test$wts <- (1/(test$y + .5)) + (1/(test$N - test$y + .5))
  
  # calculate proportion
  test$Proportion <- test$y / test$N
  
  # do an arcsine root transformation
  test$ArcSin <- asin(sqrt(test$Proportion))
  
  # set column modes
  test$ParticipantName <- factor(test$ParticipantName)
  test$Trial <- factor(test$Trial)
  
  for (i in 1:length(factors)) {
    factor <- factors[i]
    test[, factor] <- factor(test[, factor])
  }
  
  test
}

# diverging_bins
#
# data = master_test file (the entire file will be used)
# bin_size = time (in ms) for each bin
# factor = the factor by which to compare groups
# dv = the dependent variable (column of 1's and 0's)
diverging_bins <- function(data, bin_size = 250, factor = 'Condition', dv = 'Animate') {
  bin_size_in_frames <- round(bin_size / 16.666667,0)
  
  # bin by participant
  binned <- bin_data(data, 'FramesFromPhaseOnset', bin_size_in_frames, c('ParticipantName',factor), dv)
  
  # rename columns
  colnames(binned) <- c('ParticipantName',factor,'Bin','BinMean','N','y')
  
  # because we may have exclude trial time before our window, let's adjust our bin numbers
  # so they start at 0
  min_bin <- min(binned[, 'Bin'])
  
  #if (min_bin > 0) {
  #  binned$Bin <- binned$Bin - min_bin
  #}
  
  # test each consecutive bin
  bins <- max(binned$Bin)
  
  results <- data.frame(matrix(nrow = bins, ncol = 7))
  colnames(results) <- c('Bin','StartTime','EndTime','Participants','SameDifferent','p-value','ANOVA')
  
  for (bin in 0:bins) {
    bin_data <- binned[which(binned$Bin == bin), ]
    num_levels <- length(unique(bin_data[, factor]))
    
    if (num_levels <= 1) {
      next
    }
    
    frm <- paste('BinMean',factor,sep="~")
    
    aov <- aov(formula(frm), data = bin_data)
    p_value <- summary(aov)[[1]][["Pr(>F)"]]
    
    if (p_value < .05) {
      samedifferent <- 'different'
    }
    else {
      samedifferent <- 'same'
    }
    
    participants <- length(unique(factor(bin_data[, data_options$participant_factor])))
    
    results[bin + 1, ] <- c(bin, round((bin*bin_size_in_frames*16.666667),0), round(((bin*bin_size_in_frames + bin_size_in_frames)*16.666667),0), participants, samedifferent, p_value, '')
  }
  
  results
}

# bin_data ()
# 
# This function will bin values in one column, "cvar", grouping by any number of "constants" columns
# and one "timevar" column. The timevar column is divided by binsize and rounded up.
# All rows that have the same values for the new timevar column and constants columns are grouped
# together. Within each group, the mean, length, and sum are calculated on the cvar column.
# - df: The input data frame
# - timevar: The column that determines the groupings
# - binsize: The size of bins (timevar is divided by this value and rounded up)
# - constants: the columns that are kept the same
# - cvar: the variable which is processed

# Example using this data set:
# subj condition frame look
#    1    square     0    1
#    1    square     1    1
#    1    square     2    0
#    1    square     3    0
#    2  triangle     0    1
#    2  triangle     1    0
#    2  triangle     2    0
#    2  triangle     3    0
#    2  triangle     4    1
#
# bin_data(data, "frame", 2, c("subj", "condition"), "look")
# Returns the following. Note that the "frame" column is essentially the old
#  "frame" column divided by 2
# subj condition frame BinMean BinSize BinSum
#    1    square     0     1.0       2      2
#    1    square     1     0.0       2      0
#    2  triangle     0     0.5       2      1
#    2  triangle     1     0.0       2      0
#    2  triangle     2     1.0       1      1

# Example usage:
#  tb14 <- bin_data(tobii14, "FramesFromPhaseOnset", 2, c("SubjectNum", "Condition", "Trial", "Phase", "Subphase"), "GazeXAvg")
bin_data <- function(df, timevar, binsize, constants, cvar) {  
  # set all rows of the var that are NA to 0, so we don't get errors in calculating a mean
  # if we have rows like this, we have to assume they meant to keep trackloss, even though
  # keep_trackloss() would have already set NA to 0...
  df[is.na(df[,cvar]), cvar] <- 0
  
  # (Brock) after we subset the data and our "zero" point is actually, say, 8000 frames from the 
  # phase onset (using "FramesFromPhaseOnset"), we run into trouble if we ask it to bin by a number of frames
  # that does not go into the minimum value evenly.  SO, let's re-calibrate our timevar column
  # with a nice clean 0.
  if (min(df[,timevar]) > 0) {
    df[,timevar] <- df[,timevar] - min(df[,timevar])
  }
    
  # If binsize=3, then 0,1,2 will go to 0; 3,4,5 will go to 1, and so on. 
  cat(sprintf("Dividing values in column %s by bin size %d and rounding down... \n", cvar, binsize))        
  df[,timevar] <- floor(df[,timevar]/binsize)
  
  # Collapse all rows where constants+timevar columns are the same 
  cat(sprintf("Processing column %s, in groups where the following columns have the same value:\n", cvar))
  cat(sprintf("    %s\n", paste(c(constants, timevar), collapse=", ")))
  cat("    mean... ")
  df.mean   <- aggregate(df[cvar], by=df[c(constants,timevar)], mean)
  names(df.mean)[names(df.mean)==cvar]     <- "BinMean"
  
  # Get the number of rows in each bin
  cat(" bin size... ")
  df.length <- aggregate(df[cvar], by=df[c(constants,timevar)], length)
  names(df.length)[names(df.length)==cvar] <- "BinSize"
  
  # Get the sum of the rows
  cat(" sum...\n")
  df.sum    <- aggregate(df[cvar], by=df[c(constants,timevar)], sum)
  names(df.sum)   [names(df.sum)   ==cvar] <- "BinSum"
  
  cat("Merging data frames for mean, bin size, and sum...\n")
  dfb <- merge(df.mean, df.length, c(constants,timevar))
  dfb <- merge(dfb,     df.sum,    c(constants,timevar))
  
  # Sort by all columns, from left to right
  dfb <- dfb[ do.call(order, as.list(dfb)), ]
  
  dfb
}

# plot_data()
#
# Wrap spaghetti() to simplify it
#
plot_data <- function(data, output_file = 'graph.png', dv = 'Animate', factor = 'Condition', title = 'Looking', y_title = 'Proportion', x_title = 'Time (ms)', type = 'empirical', vertical_lines = c(), bin_size = 100, x_gap = 500, width = 1000, height = 600) {
  require('ggplot2', quietly = TRUE)
  require('ggthemes', quietly = TRUE)
  require('reshape2', quietly = TRUE)
  
  data$TimeAlign <- data$TimeFromMovieOnset - min(data$TimeFromMovieOnset)
  
  data <- aggregate(data.frame(data[, dv]), by = list(data[, data_options$participant_factor], data[, factor], data$TimeAlign), FUN = mean)  
  colnames(data) <- c('ParticipantName',factor,'TimeAlign',dv)
  
  spaghetti(
      data,
      fname=output_file,
      onset=0,
      addOnsetLine=F,
      plotwidth=width,
      plotheight=height,
      meltids=c("ParticipantName","TimePlot",factor),
      meltfactors=c(factor),
      timeAlignVar="TimeAlign",
      colorvariable=factor,
      errbarvars=NA,
      srate=16.6666667, # change this if we ever get a higher resolution Tobii
      region=c(dv),
      sample=bin_size,
      downsample="bin",
      upperlimit=max(data$TimeAlign),
      xbreakstep=x_gap,
      plottype=type,
      graphTitle=title,
      y_title = y_title,
      x_title = x_title
    )
}

# spaghetti()
#
# Plot eyetracking data in a spaghetti plot
#
spaghetti = function(
  # version 0.1
  # written by jdegen@bcs.rochester.edu
  # 01/11/2012
  # modified by Brock for this library
  
  # TODO:
  # 1. Implement plotting of multiple regions in one plot. Currently only plotting one region at a time (by conditions) is possible. But sometimes we want to plot looks to eg target/competitor/distractor in one plot.
  # 2. Give users more control over where to add vertical lines. So far, only adding one vertical line relative to one event and another (or group of others) relative to another event. Important is that there's a time column for each of these events. Ideally, you would also have the option of passing a vector of times, where each designates an x intercept. Or a combination of the two.
  # 3. Add shapevariable as an option (important for writing papers where you can only do black and white prints and you want to use shapes to distinguish conditions).
  # 4. Add "TimePlot" to vector of meltids so user doesn't have to do it
  # 5. Generate meltfactors automatically by probing the type of variables in meltids
  # 6. If errbarvars is NA, create vector automatically by creating vector of all of {color/line/size/facet}variable that aren't NA
  
  # assumes that there are no rows where the value in the column coding region that is currently being looked at is empty. ie for ExAnalysis data.frames: rp_RegionType != ""
  data,
  meltids=c("TimeFromMovieOnset"), # vector of column names (as strings) to melt the dataset by (i.e. that you want to have the option of plotting by)
  meltfactors, # the variables in meltids that are factors. Necessary for as.factor(as.character) calls  	
  region=c("Target"), # vector of regions - region names must be column names in data
  colorvariable=NA, # variable you want to assign different colors by
  colormapping=NA, # a data.frame with two factor columns. One column with name of variable you want to map different colors to. One called "Color" with the color for each factor level. Data.frame row names must be levels of mapping variable, and data.frame must be sorted by mapping variable. 
  linevariable=NA, # variable (column name in data), levels of which you want to plot in different line types. Currently support only for two levels.
  sizevariable=NA, # variable (column name in data), levels of which you want to plot in different sizes. you can't specify a sizevariable if you haven't specified a linevariable. Currently support only for two levels.
  facetvariable=NA, # variable (column name in data), levels of which you want to plot in different grids. uses facet_wrap.
  errbars=TRUE, # plot error bars by default	
  errbarvars=NA, # vector of column names (as strings) in the melted dataset, the combination of which error bars are to be calculated from (only relevant if drawing ribbons/error bars)
  dataid="DataID", # name of the variable in the data.frame that uniquely codes samples. downsampling assumes that the data.frame is ordered by this variable, and that sample i and sample i+1 are in fact taken at sampling points t and t+1 (where t is the time step defined by the sampling frequency of the tracker, e.g. 4ms is the default for the EyeLink 500)
  sample=20, # ms to downsample to
  downsample="down", # down: downsample. bin: bin data into time bins of size sample
  srate=4, # sampling rate in ms. 4ms is the default for the EyeLink 500	
  half="both", # only generalized for half="both". if half = "first" or "second", assumes number of trials in my color_gumballs dataset
  exclude=NA, # don't exclude data by default. if exclude is list of vectors of character strings, interpret character strings as levels of the variables denoted by the names of the list elements and exclude those from data. e.g. exclude=list(Var1=c("a","b"),Var2=c("1")). only works with factors.
  onset = 100, # time that 0-point of linguistic event of interest should be aligned to, in ms
  upperlimit = 5133, # upper limit on x axis (time in ms)
  timeAlignVar=NA, # column that contains time variable that you want to align by (ie you want to align at the 0 point of that variable)
  addOnsetLine=TRUE, # add a vertical line at linguistic event onset
  addVerticalLine=FALSE, # add a second vertical line at eg mean onset of another event. provide name of that time variable where 0 is the onset of that event, e.g. "Time_rel_stim_Adjective"
  extraVerticalType="collapsed", # one of "individual" or "collapsed". Either prints mean onset of each level of the additional linguistic event of interest (individual) or overall mean (sollapsed).
  #	onsetReg=c("competitor","target"), # regions that can have been looked to at onset of linguistic event of interest
  plottype="empirical", # one of "empirical" or "smoothed". The former plots empirical means (without error bars), the latter plots smoothed predicted means based on y~x. If you want to change the formula for the smoother, set form.
  form=formula("y~x"), # formula to pass to smoother. 
  method = "auto", # method for smoother to use. can be e.g. lm, glm, loess, etc. see stat_smooth for details
  #	aggregateby=FALSE, # if downsample == "bin" and aggregateby is a string, proportions will be computed for timewindows of size sample and aggregated by aggregateby (eg "Subject")
  analysiswindow=FALSE, # don't plot analysis window by default. when TRUE, not yet generalized. maybe add extra argument of c(window_start,window_end) times, relative to onset of linguistic event of interest?
  analysisonset=0, 
  extraopts=FALSE, # don't change the text size etc defaults. when TRUE, sets legend text size to 20, axis text size to 15, axis title size to 20, legend background to white, and panel label size to 20
  #	align="qonset", # figure out if you can generalize this at all
  xbreakstep=200, # size of tick mark spacing on x-axis
  plotwidth=800, # width of the plot in pixels
  plotheight=450, # height of the plot in pixels
  fname=NA, # file name (quoted). if left NA, generates a filename for pdf that is not generalized	
  fileformat="png", # can be one of "png" or "pdf"
  showplot=FALSE, # don't print plot by default (save straight to file instead). If TRUE, will both print plot in quartz window and print plot to file
  
  # parameters deleted by Brock
  # graphdir="./", # path to directory where graph will be saved
  
  # parameters added by Brock
  graphTitle = "Looking Proportions",
  y_title = 'Proportion Looking',
  x_title = 'Time (ms)',
  timeAdjust = 0,
  vertical_lines = NA, # e.g., c(15000,20000)
  ...
)
{
  # remove data based on specified factor levels	
  if (!is.na(exclude))
  {
    for (e in names(exclude))
    {
      data = data[!data[,e] %in% exclude[[e]],]
      print(paste("removed levels:",exclude[[e]],"of variable",e))
    }
    print(paste("after removing factor levels:",nrow(data)))
  }
  
  # remove first/second half
  if (half == "first")
  {
    data <- subset(data, TrialNumber <= 120/2)
    print(paste("after removing second half:",nrow(data)))
  }
  if (half == "second")
  {
    data <- subset(data, TrialNumber > 120/2)
    print(paste("after removing first half:",nrow(data)))		
  }		
  
  # create time variable to plot on x axis and remove samples not in desired time window
  print(paste("event onset at", onset))
  data$TimePlot = data[,timeAlignVar] + onset
  
  if (addVerticalLine != FALSE) { data$NewAdj = onset + (data[,timeAlignVar] - data[,addVerticalLine]) }
  data = subset(data, TimePlot >= 0 & TimePlot <= upperlimit)
  print(paste("after removing samples before sentence onset and after upper limit:",nrow(data)))
  
  if (downsample == "bin")
  {
    print(paste("binning into time bins of",sample,"ms"))
    data$TimePlot <- (data$TimePlot %/% sample) * sample
    print(paste("after binning data:",nrow(data)))		
  } else {
    if (downsample == "down")	
    {
      print(paste("downsampling to",sample,"ms"))	
      data <- data[as.numeric(as.character(data[,"TimePlot"])) %% (sample/srate) == 0,]
      print(paste("after downsampling:",nrow(data)))			
    }
  }
  
  if (is.na(fname)) 
  { 
    if (fileformat == "png")
    {
      fname = "spaghettiplot.png" 
    } else {
      if (fileformat == "pdf")	
      {
        fname = "spaghettiplot.pdf"
      }
    }	
  }
  
  # create a reduced dataset containing only the data you need
  for (me in meltfactors)
  {
    data[,me] = as.factor(as.character(data[,me]))
  }
  melted = melt(data,id = meltids,measure=region)
  melted$value = as.numeric(as.character(melted$value))
  
  i=0
  texty=c()
  
  if (is.na(errbarvars))
  {
    errbarvars = c(colorvariable,linevariable,sizevariable,facetvariable)
    errbarvars = errbarvars[!is.na(errbarvars)]
  }
  
  for (v in errbarvars)
  {
    if (i==0)
    {
      texty=paste(texty,paste("data[,\"",v,"\"]",sep=""),sep="")
      i=1
    } else {
      texty=paste(texty,paste("data[,\"",v,"\"]",sep=""),sep=",")			
    }
  }
  texty=paste("paste(",texty,")",sep="")
  melted$errbargroup = eval(parse(text=texty))
  melted$errbargroup = as.factor(as.character(melted$errbargroup))
  
  # aggregate over subjects so they don't contribute multiple points to a given bin
  melted <- with(melted,aggregate(value, by = list(ParticipantName,TimePlot,eval(parse(text=colorvariable)),variable,errbargroup), FUN = mean))
  colnames(melted) <- c('ParticipantName','TimePlot',colorvariable,'variable','errbargroup','value')
  
  # create the base plot
  if (is.na(facetvariable))
  {
    if (plottype == "empirical")
    {
      if (is.na(linevariable))
      {
        agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup),FUN="mean"))
        colnames(agr) = c("TimePlot",colorvariable,"errbargroup","value")		
        
        agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup),FUN="se"))$x
        agr$YMin = agr$value - agr$SE
        agr$YMax = agr$value + agr$SE
        limits = aes(ymin=YMin,ymax=YMax)					
        p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable))
      } else {			
        if (is.na(sizevariable))
        {
          agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup),FUN="mean"))
          colnames(agr) = c("TimePlot",colorvariable,linevariable,"errbargroup","value")
          
          agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup),FUN="se"))$x	
          agr$YMin = agr$value - agr$SE
          agr$YMax = agr$value + agr$SE
          limits = aes(ymin=YMin,ymax=YMax)		
          p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable))
        } else {
          agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup),FUN="mean"))
          colnames(agr) = c("TimePlot",colorvariable,linevariable,sizevariable,"errbargroup","value")
          
          agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup),FUN="se"))$x
          agr$YMin = agr$value - agr$SE
          agr$YMax = agr$value + agr$SE
          limits = aes(ymin=YMin,ymax=YMax)						
          
          p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable)) +
            scale_size_manual(values=c(3,1.5))	
        }
      }
      p = p +
        geom_line(aes_string(group="errbargroup"),size=I(2))
      if (errbars)
      {
        p = p + geom_errorbar(limits,linetype=I(1))	
      }
      
    } else
    {
      if (is.na(linevariable))
      {
        p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable))
      } else {
        if (is.na(sizevariable))
        {
          p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable))			
        } else {
          p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable))						
        }
      }
      p = p +
        stat_smooth(aes_string(group="errbargroup",fill=colorvariable),size=I(1.5),method=method,formula=form)
      if (!is.na(colormapping)) {
        p = p + scale_fill_manual(values=colormapping[sort(as.character(unique(melted[,colorvariable]))),]$Color)
      }				
    }
  } else {
    if (plottype == "empirical")
    {
      if (is.na(linevariable))
      {
        agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup,eval(parse(text=facetvariable))),FUN="mean"))
        colnames(agr) = c("TimePlot",colorvariable,"errbargroup",facetvariable,"value")		
        agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),errbargroup,eval(parse(text=facetvariable))),FUN="se"))$x
        agr$YMin = agr$value - agr$SE
        agr$YMax = agr$value + agr$SE
        limits = aes(ymin=YMin,ymax=YMax)					
        p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable)) #+
      } else {			
        if (is.na(sizevariable))
        {
          agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="mean"))
          colnames(agr) = c("TimePlot",colorvariable,linevariable,"errbargroup",facetvariable,"value")
          agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="se"))$x	
          agr$YMin = agr$value - agr$SE
          agr$YMax = agr$value + agr$SE
          limits = aes(ymin=YMin,ymax=YMax)		
          p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable)) #+
        } else {
          agr <- with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="mean"))
          colnames(agr) = c("TimePlot",colorvariable,linevariable,sizevariable,"errbargroup",facetvariable,"value")
          agr$SE = with(melted, aggregate(value,by=list(TimePlot,eval(parse(text=colorvariable)),eval(parse(text=linevariable)),eval(parse(text=sizevariable)),errbargroup,eval(parse(text=facetvariable))),FUN="se"))$x
          agr$YMin = agr$value - agr$SE
          agr$YMax = agr$value + agr$SE
          limits = aes(ymin=YMin,ymax=YMax)						
          
          p = ggplot(agr, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable)) +
            scale_size_manual(values=c(3,1.5))	
        }
      }
      p = p +
        geom_line(aes(group=errbargroup),size=I(1))
      if (errbars)
      {
        p = p + geom_errorbar(limits,linetype=I(1))	
      }
      
    } else
    {
      if (is.na(linevariable))
      {
        p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable))
      } else {
        if (is.na(sizevariable))
        {
          p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable))			
        } else {
          p = ggplot(melted, aes_string(x="TimePlot",y="value",color=colorvariable,linetype=linevariable,size=sizevariable))						
        }
      }
      p = p +
        stat_smooth(aes_string(group="errbargroup",fill=colorvariable),size=I(1.5))
      if (!is.na(colormapping)) {
        p = p + scale_fill_manual(values=colormapping[sort(as.character(unique(melted[,colorvariable]))),]$Color)			
      }	
    }
    form = as.formula(paste("~",facetvariable,sep=""))
    p = p +
      facet_wrap(form)			
  }
  
  # extra stuff that's common to all plots
  if (analysiswindow) { onset = onset + analysisonset }
  
  p <- p + theme_few()
  
  p <- p + scale_y_continuous(y_title) +
    coord_cartesian(ylim=c(0.0,1.0)) +	
    scale_y_continuous(y_title, breaks = seq(0,1,by=.1)) +
    scale_x_continuous(x_title,breaks=seq(0 + timeAdjust,upperlimit + timeAdjust,by=xbreakstep)) 
  if (addOnsetLine)
  {
    p = p + geom_vline(xintercept=onset, colour="black",size=I(1.5),legend=FALSE)
  }
  if (!is.na(colormapping))	
  {
    p = p +	
      scale_colour_manual(values=colormapping[sort(as.character(unique(melted[,colorvariable]))),]$Color)
    
  }
  
  if (addVerticalLine != FALSE)
  {
    means <- with(data,aggregate(NewAdj, by=list(eval(parse(text=colorvariable))), FUN=mean))
    row.names(means) <- means$Group.1
    means <- means[order(means$Group.1),]
    
    if (extraVerticalType == "individual")
    {
      for (i in 1:length(means$x))
      {
        xi <- means$x[i]
        if (analysiswindow) { xi <- xi + analysisonset }
        if (!is.na(colormapping))
        {
          co <- as.character(colormapping[as.character(means$Group.1[i]),]$Color)
        } else {
          co <- "black"
        }
        p <- p + geom_vline(xintercept=xi, colour=I(co),size=I(1.5),legend=FALSE)
      }
    } else {
      if (extraVerticalType == "collapsed")	
      {
        xi <- mean(means$x)				
        if (analysiswindow) { xi <- xi + analysisonset }
        p <- p + geom_vline(xintercept=xi, colour="black", size=I(1.5),legend=FALSE)
      }
    }
  }
  
  
  # set options
  p <- p +
    opts(
      legend.text = theme_text(size = 12),
      axis.text.x = theme_text(size=12),
      axis.text.y = theme_text(size=12,hjust=1),
      axis.title.x = theme_text(size=14,vjust=.05),
      axis.title.y = theme_text(size=14,angle=90,vjust=0.25),
      legend.background=theme_rect(fill="white"),
      strip.text.x = theme_text(size=14),
      title = graphTitle,
      plot.title = theme_text(size=18, lineheight=.8, face="bold", vjust=1.5)
    ) 
  
  if (!is.na(vertical_lines)) {
    for (i in 1:length(vertical_lines)) {
      line <- vertical_lines[i]
      
      cat("line: ")
      cat(line)
      
      p = p + geom_vline(xintercept=line, colour="black",size=I(1.5))
    }
  }
  
  if (showplot) { print(p) }
  # print plot to file	
  print(paste("printing to",fname))	
  if (fileformat == "png")
  {
    png(file=fname, width=plotwidth, height=plotheight)
  } else {
    if (fileformat == "pdf")
    {
      pdf(file=fname, width=plotwidth, height=plotheight)
    }
  }
  print(p)
  dev.off()
  
  rm(p)
  gc()
  
}

se <- function(x) {
  y <- x[!is.na(x)] # remove the missing values, if any
  sqrt(var(as.vector(y))/length(y))
}

error <- function(method, message) {
  stop(paste("(",method,"): ",message,"\n",sep=""), call. = FALSE)
}

message <- function(method, message) {
  cat(paste(message,"\n",sep=""))
}