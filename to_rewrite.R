# time_analysis()
#
# Create a dataset that is ready for time-based analyses, either a linear
# analysis or a non-linear (i.e., growth curve) analysis
#
# @param dataframe data
# @param list data_options
# @param integer bin_time The time (ms) to fit into each bin
# @param string dv The dependent variable column
# @param character.vector A vector of factor columns
#
# @return list(bySubj, byItem, crossed)
time_analysis <- function (data, data_options, bin_time = 250, dv = NA, factors = NA) {
  # use defaults if unset
  if (is.na(dv)) {
    dv = data_options$default_dv
  }
  
  if (length(factors) < 2 && is.na(factors)) {
    factors = data_options$default_factors
  }
  
  # make sure the factor columns are actually factors for the sake of binning
  # but track the classes so we can reset these afterwards...
  original_classes = list()
  
  for (i in 1:length(factors)) {
    factor_name = factors[i]
    
    # save original class
    # if its character/factor, we will leave as a factor later
    # if its numeric, we will return it to its numeric state
    original_classes[factor_name] = class(data[, factor_name])
    
    # now factor!
    data[, factor_name] <- factor(data[, factor_name])
  }
  
  # ====== create bySubj data frame
  
  # calculate the number of samples to place in a bin based on
  # the amount of time (ms) we want in a bin (bin_time)
  bin_size_in_samples <- round(bin_time / (1000 / data_options$sample_rate),0)
  
  # bin by participant and factors across time
  binned <- bin_data(data, data_options, bin_size_in_samples, c(data_options$participant_factor,factors), dv)
  
  # rename columns
  colnames(binned) <- c(data_options$participant_factor,factors,'Bin','BinMean','N','y')
  
  # because we may have excluded trial time before our window, let's re-calibrate
  # our bin numbers to a zero point
  min_bin <- min(binned[, 'Bin'])
  
  if (min_bin > 0) {
    binned$Bin <- binned$Bin - min_bin
  }
  
  # create t1
  binned$t1 <- binned$Bin * (bin_time / 1000)
  
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
  
  # ====== create byItem data frame
  
  # calculate the number of samples to place in a bin based on
  # the amount of time (ms) we want in a bin (bin_time)
  bin_size_in_samples <- round(bin_time / (1000 / data_options$sample_rate),0)
  
  # bin by participant and factors across time
  binned <- bin_data(data, data_options, bin_size_in_samples, c(data_options$participant_factor,factors), dv)
  
  # rename columns
  colnames(binned) <- c(data_options$trial_factor,factors,'Bin','BinMean','N','y')
  
  # because we may have exclude trial time before our window, let's adjust our bin numbers
  # so they start at 0
  min_bin <- min(binned[, 'Bin'])
  
  if (min_bin > 0) {
    binned$Bin <- binned$Bin - min_bin
  }
  
  # create t1
  binned$t1 <- binned$Bin * (bin_time / 1000)
  
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
  
  # ====== create crossed (by Items and Subjects) data frame
  
  # calculate the number of samples to place in a bin based on
  # the amount of time (ms) we want in a bin (bin_time)
  bin_size_in_samples <- round(bin_time / (1000 / data_options$sample_rate),0)
  
  # bin by participant and factors across time
  binned <- bin_data(data, data_options, bin_size_in_samples, c(data_options$participant_factor,data_options$trial_factor,factors), dv)
  
  # rename columns
  colnames(binned) <- c(data_options$participant_factor, data_options$trial_factor,factors,'Bin','BinMean','N','y')
  
  # because we may have exclude trial time before our window, let's adjust our bin numbers
  # so they start at 0
  min_bin <- min(binned[, 'Bin'])
  
  if (min_bin > 0) {
    binned$Bin <- binned$Bin - min_bin
  }
  
  # create linear time column
  binned$t1 <- binned$Bin * (1000 / bin_time)
  
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
  
  # rename as crossed
  crossed <- binned
  
  # ======== reset original column classes, if possible
  
  # above, we recorded original factor classes in 'original_classes'
  # if these are numeric, let's reset them
  # if they were characters/factors, we can leave them as factors for the sake
  # of analysis
  
  for (factor_name in names(original_classes)) {
    original_class <- original_classes[[factor_name]]  
    if (original_class == 'numeric') {
      bySubj[, factor_name] <- as.numeric(as.character(bySubj[, factor_name]))
      byItem[, factor_name] <- as.numeric(as.character(byItem[, factor_name]))
      crossed[, factor_name] <- as.numeric(as.character(crossed[, factor_name]))
    }  
    else {
      bySubj[, factor_name] <- factor(bySubj[, factor_name])
      byItem[, factor_name] <- factor(byItem[, factor_name])
      crossed[, factor_name] <- factor(crossed[, factor_name])
    }
  }
  
  list (
      'bySubj' = bySubj,
      'byItem' = byItem,
      'crossed' = crossed
    )
}




# bootstrapped_splines(data, data_options, factor, within_subj, samples, resolution, alpha)
#
# Bootstrap splines from a time_analysis() file. Returns a plottable time series dataframe with means for each
# level of Factor (i.e., each condition), as well as a time series dataframe which shows the points of divergence
# throughout the time window.
#
# @param dataframe data Your clean dataset
# @param list data_options Standard list of options for manipulating dataset
# @param string factor What factor to split by? Maximum two conditions!
# @param boolean within_subj Are the two conditions within or between subjects?
# @param int samples How many (re)samples to take?
# @param float resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100 intervals per second, or hundredths of a second
# @param float alpha p-value when the groups are sufficiently "diverged"
#
# @return list(samples, divergence)
bootstrapped_splines <- function (data, data_options, factor = '', within_subj = FALSE, samples=1000, resolution=10, alpha = .05) {
  # dependencies
  library(dplyr, quietly=TRUE)
  library(data.table, quietly=TRUE)
  
  # define sampler for splines...
  spline_sampler <- function (dataframe, data_options, resolution) {
    if (rbinom(1,1,.1) == 1) {
      cat('.')
    }
    
    run_original <- dataframe
    
    # get subjects
    run_subjects <- levels(run_original[, data_options$participant_factor])
    
    # get timepoints
    run_times <- unique(run_original$t1)
    run_times <- run_times[order(run_times)]
    
    # randomly sample N subjects (with replacement from data)
    run_sampled <- sample(run_subjects, length(run_subjects), replace = TRUE)
    
    # create a dataset of ParticipantName,t1,BinMean for each sampled subject (including duplicates)
    run_rows <- length(run_sampled) * length(run_times)
    run_data <- data.frame(matrix(nrow=run_rows,ncol=2))
    
    # use data.table's setnames for speed increase
    #colnames(run_data) <- c(data_options$participant_factor,'t1')
    setnames(run_data,c(data_options$participant_factor,'t1'))
    
    run_data[, data_options$participant_factor] <- rep(run_sampled, each=length(run_times))
    run_data[, data_options$participant_factor] <- factor(run_data[, data_options$participant_factor])
    
    run_data$t1 <- rep(run_times, times=length(run_subjects))
    
    # replaced merge with inner_join() for speed increase
    #run_data <- merge(run_data, run_original[, c(data_options$participant_factor,'t1','BinMean')], by = c(data_options$participant_factor,'t1'))
    run_data <- inner_join(run_data, run_original[, c(data_options$participant_factor,'t1','BinMean')], by = c(data_options$participant_factor,'t1'))
    
    # spline! 
    # with generalized cross-validation setting smoothing parameter
    run_spline <- with(run_data, smooth.spline(t1, BinMean, cv=FALSE))
    
    # get interpolated spline predictions for total time at .01 (hundredth of a second scale)
    run_predicted_times <- seq(min(run_times), max(run_times), by=resolution)
    run_predictions <- predict(run_spline, run_predicted_times)
    
    run_predictions$y
  }
  
  # process arguments
  
  # convert resolution to seconds
  resolution = resolution / 1000;
  
  # validate variables
  if (factor == '') {
    error('bootstrapped_splines','No factor specified. Must specify a factor with 2 levels.')
  }
  
  if (length(levels(data[, factor])) != 2) {
    error('bootstrapped_splines','Factor must have 2 levels.')
  }
  
  # this dataframe will hold our final dataset
  combined_bootstrapped_data <- data.frame()
  
  # re-factor Participant name column, so that levels() is accurate
  data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
  
  # between-subjects:
  if (within_subj == FALSE) {
    for (label in levels(data[, factor])) {
      subsetted_data <- data[which(data[, factor] == label), ]
      
      cat('Sampling')
      bootstrapped_data <- replicate(samples, spline_sampler(subsetted_data, data_options, resolution))
      bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))
      
      sample_rows <- paste('Sample', c(1:samples), sep="")
      
      # use data.table's setnames for speed
      #colnames(bootstrapped_data) <- sample_rows
      setnames(bootstrapped_data, sample_rows)
      
      bootstrapped_data[, factor] <- label
      bootstrapped_data$t1 <- seq(min(subsetted_data$t1), max(subsetted_data$t1), by=resolution)
      bootstrapped_data$mean <- 0
      bootstrapped_data$se <- 0
      bootstrapped_data$CI025 <- 0
      bootstrapped_data$CI975 <- 0
      
      bootstrapped_data <- bootstrapped_data[, c(factor,'t1','mean','se','CI025','CI975',sample_rows)]
      
      # se is the SD of the sample
      bootstrapped_data$se <- apply(bootstrapped_data[, sample_rows], 1, sd)
      bootstrapped_data$mean <- apply(bootstrapped_data[, sample_rows], 1, mean)
      bootstrapped_data$CI025 <- bootstrapped_data$mean - 1.96*bootstrapped_data$se
      bootstrapped_data$CI975 <- bootstrapped_data$mean + 1.96*bootstrapped_data$se
      #bootstrapped_data$CI025 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.025) })
      #bootstrapped_data$CI975 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.975) })
      
      # using dplyr's rbind_list or speed increase
      #combined_bootstrapped_data <- rbind(combined_bootstrapped_data,bootstrapped_data)
      combined_bootstrapped_data <- rbind_list(combined_bootstrapped_data,bootstrapped_data)
    }
  }
  else {
    # within-subjects:
    
    # for within-subjects, we need to calculate the difference between
    # level 1 and 2 of the factor for each subject before sampling splines
    library(reshape2)
    data <- dcast(data, as.formula(paste(paste(data_options$participant_factor,'t1',sep=" + "),factor,sep=' ~ ')), value.var='BinMean', fun.aggregate = mean, drop = TRUE)
    
    # re-calculate BinMean as the DIFFERENCE between the two labels
    data$BinMean <- data[, 3] - data[, 4]
    
    # remove all samples where BinMean == NA
    data <- data[!is.na(data$BinMean), ]
    
    data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
    
    cat('Sampling')
    bootstrapped_data <- replicate(samples, spline_sampler(data, data_options, resolution))
    bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))
    
    sample_rows <- paste('Sample', c(1:samples), sep="")
    
    # use data.table for speed increase
    #colnames(bootstrapped_data) <- sample_rows
    setnames(bootstrapped_data, sample_rows)
    
    bootstrapped_data$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
    bootstrapped_data$mean <- 0
    bootstrapped_data$se <- 0
    bootstrapped_data$CI025 <- 0
    bootstrapped_data$CI975 <- 0
    
    bootstrapped_data <- bootstrapped_data[, c('t1','mean','se','CI025','CI975',sample_rows)]
    
    # se is the SD of the sample
    bootstrapped_data$se <- apply(bootstrapped_data[, sample_rows], 1, sd)
    bootstrapped_data$mean <- apply(bootstrapped_data[, sample_rows], 1, mean)
    bootstrapped_data$CI025 <- bootstrapped_data$mean - 1.96*bootstrapped_data$se
    bootstrapped_data$CI975 <- bootstrapped_data$mean + 1.96*bootstrapped_data$se
    #bootstrapped_data$CI025 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.025) })
    #bootstrapped_data$CI975 <- apply(bootstrapped_data[, sample_rows], 1, function (x) { quantile(x,probs=.975) })
    
    # use dplyr's rbind_list for speed increase
    #combined_bootstrapped_data <- rbind(combined_bootstrapped_data,bootstrapped_data)
    combined_bootstrapped_data <- rbind_list(combined_bootstrapped_data,bootstrapped_data)
  }
  
  # between-subjects:
  
  if (within_subj == FALSE) {
    bootstrapped_diverged <- combined_bootstrapped_data[, c(factor,'t1','mean','se')]
    
    # first, we need to calculate the difference between the two groups at each point
    library(reshape2)
    
    # get mean for each level of factor
    bootstrapped_diverged_mean <- dcast(bootstrapped_diverged, paste('t1',factor,sep=' ~ '), value.var = 'mean', fun.aggregate = mean, drop = TRUE)
    
    # get se for each level of factor
    bootstrapped_diverged_se <- dcast(bootstrapped_diverged, paste('t1',factor,sep=' ~ '), value.var = 'se', fun.aggregate = mean, drop = TRUE)
    colnames(bootstrapped_diverged_se) <- c('t1','Group1SE','Group2SE')
    bootstrapped_diverged <- cbind(bootstrapped_diverged_mean, bootstrapped_diverged_se[, c('Group1SE','Group2SE')])
    rm(bootstrapped_diverged_se)
    
    # calculate SD
    N <- length(levels(data[, data_options$participant_factor]))
    
    bootstrapped_diverged$Group1SD <- bootstrapped_diverged$Group1SE*sqrt(N)
    bootstrapped_diverged$Group2SD <- bootstrapped_diverged$Group2SE*sqrt(N)
    
    bootstrapped_diverged$PooledSDSquared <- (bootstrapped_diverged$Group1SD^2 + bootstrapped_diverged$Group2SD^2) / 2
    bootstrapped_diverged$PooledSE <- sqrt(bootstrapped_diverged$PooledSDSquared)*sqrt((1/N) + (1/N))
    
    bootstrapped_diverged$t <- (abs(bootstrapped_diverged[, 3] - bootstrapped_diverged[, 2])) / bootstrapped_diverged$PooledSE
    
    required_t <- abs(qt((alpha/2), (N*2)-2))
    
    bootstrapped_diverged$Diverged <- 0
    bootstrapped_diverged[which(bootstrapped_diverged$t >= required_t), 'Diverged'] <- 1
  }  
  else {
    bootstrapped_diverged <- combined_bootstrapped_data[, c('t1','mean','se')]
    
    # within-subjects
    # just do a comparison to 0 for difference (mean) and standard error
    
    bootstrapped_diverged$t <- bootstrapped_diverged$mean / bootstrapped_diverged$se
    
    N <- length(levels(data[, data_options$participant_factor]))
    required_t <- abs(qt((alpha/2), (N*2)-2))
    
    bootstrapped_diverged$Diverged <- 0
    bootstrapped_diverged[which(bootstrapped_diverged$t >= required_t), 'Diverged'] <- 1
  }
  
  # return everything!
  list(
    samples = combined_bootstrapped_data,
    divergence = bootstrapped_diverged
  )
}

# bootstrapped_splines_nonparametric(data, data_options, factor, within_subj, samples, resolution, alpha)
#
# Bootstrap splines from a time_analysis() file. Returns a plottable time series dataframe with means for each
# level of Factor (i.e., each condition), as well as a time series dataframe which shows the points of divergence
# throughout the time window.
#
# PROBLEM: The standard errors returned by this function are questionable because they assume
# that each participant is included at each time point. In some sense, they are (after the
# smoothing spline is applied), but it makes spurious VISIBLE divergences at the beginnings/ends of trials
# (when few participants are looking) much more likely. Importantly, the bootstrapping
# procedure means that these apparent divergences are not actually significant, but it's a bit
# confusing because visually they look significant.
#
# @param dataframe data Your clean dataset
# @param list data_options Standard list of options for manipulating dataset
# @param string factor What factor to split by? Maximum two conditions!
# @param boolean within_subj Are the two conditions within or between subjects?
# @param int samples How many (re)samples to take?
# @param float resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100 intervals per second, or hundredths of a second
# @param float alpha p-value when the groups are sufficiently "diverged"
#
# @return list(samples, divergence)
bootstrapped_splines_nonparametric <- function (data, data_options, factor = '', within_subj = FALSE, samples=1000, resolution=10, alpha = .05) {
  # dependencies
  library(plyr, quietly=TRUE)
  #library(data.table, quietly=TRUE)
  
  # define spline function
  fit_splines <- function (data) {
    splines_set <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(length(unique(data$ParticipantName)) + 3)))
    colnames(splines_set) <- c('t1','Mean','SE',as.vector(unique(data$ParticipantName)))
    splines_set$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
    
    for (participant in unique(data$ParticipantName)) {
      spline_model <- with(subset(data, ParticipantName == participant), smooth.spline(t1, BinMean, cv=FALSE))
      
      # get interpolated spline predictions for total time at desired resolution
      splines_set[, participant] <- predict(spline_model, splines_set$t1)$y
    }
    
    # we now have splines with the spline model for each participant
    # calculate Mean and SE (i.e., SD/sqrt(N)) at each timepoint
    
    splines_set$Mean <- apply(splines_set[, as.vector(unique(data$ParticipantName))], 1, function (x) { mean(x) })
    splines_set$SE <- apply(splines_set[, as.vector(unique(data$ParticipantName))], 1, function (x) { sd(x) / sqrt(length(x)) })
    
    splines_set
  }
  
  # convert resolution to seconds
  resolution = resolution / 1000;
  
  # validate variables
  if (factor == '') {
    error('bootstrapped_splines','No factor specified. Must specify a factor with 2 levels.')
  }
  
  if (length(levels(data[, factor])) != 2) {
    error('bootstrapped_splines','Factor must have 2 levels.')
  }
  
  # re-factor Participant name column, so that levels() is accurate
  data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
  
  # between-subjects:
  if (within_subj == FALSE) {
    splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(length(unique(data$ParticipantName)) + 6)))
    colnames(splines) <- c('t1','Mean1','SE1','Mean2','SE2','MeanDifference',as.vector(unique(data$ParticipantName)))
    splines$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
    
    label_number <- 1
    for (label in levels(data[, factor])) {
      subsetted_data <- data[which(data[, factor] == label), ]
      
      # fit splines for each participant
      spline_fits <- fit_splines(subsetted_data)
      
      # add to main splines dataframe
      splines[, c(paste('Mean',label_number,sep=''), paste('SE',label_number,sep=''), as.vector(unique(subsetted_data$ParticipantName)))] <- spline_fits[, c('Mean','SE',as.vector(unique(subsetted_data$ParticipantName)))]
      
      # increment label_number
      label_number <- label_number + 1
    }
    
    # calculate MeanDifference at each point
    splines$MeanDifference <- splines$Mean2 - splines$Mean1
    
    # we now have our spline fit for the real data
    # let's random shuffle the condition labels and see how likely these divergences are
    # by chance
    
    # this dataframe will hold each of the mean differences between conditions calculated
    # at each timepoint after shuffling condition labels (i.e., by random chance)
    bootstrapped_splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(samples + 4)))
    colnames(bootstrapped_splines) <- c('t1','RealDifference','p_value','Diverged',paste('Sample',c(1:samples),sep=''))
    bootstrapped_splines$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
    bootstrapped_splines$RealDifference <- splines$MeanDifference
    
    for (i in 1:samples) {
      # shuffle conditions
      participants_assignments <- ddply(data, c('ParticipantName',factor), summarize, Mean = mean(BinMean))
      participants_assignments[, factor] <- sample(participants_assignments[, factor], length(participants_assignments[, factor]))
      
      for (participant in as.vector(unique(data$ParticipantName))) {
          new_condition <- participants_assignments[which(participants_assignments$ParticipantName == participant), factor]
          data[which(data$ParticipantName == participant), factor] <- new_condition
      }
      
      shuffled_splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(length(unique(data$ParticipantName)) + 6)))
      colnames(shuffled_splines) <- c('t1','Mean1','SE1','Mean2','SE2','MeanDifference',as.vector(unique(data$ParticipantName)))
      
      label_number <- 1
      for (label in levels(data[, factor])) {
        subsetted_data <- data[which(data[, factor] == label), ]
        
        # fit splines for each participant
        spline_fits <- fit_splines(subsetted_data)
        
        # add to main splines dataframe
        shuffled_splines[, c(paste('Mean',label_number,sep=''), paste('SE',label_number,sep=''), as.vector(unique(subsetted_data$ParticipantName)))] <- spline_fits[, c('Mean','SE',as.vector(unique(subsetted_data$ParticipantName)))]
        
        # increment label_number
        label_number <- label_number + 1
      }
      
      shuffled_splines$MeanDifference <- shuffled_splines$Mean2 - shuffled_splines$Mean1
      
      # now add to bootstrapped_splines where we keep track of these random differences
      bootstrapped_splines[, paste('Sample',i,sep='')] <- shuffled_splines$MeanDifference
    }
    
    # now how many times did a divergence this big happen by random chance with shuffled labels?
    bootstrapped_splines$p_value <- apply(bootstrapped_splines[, c('RealDifference',paste('Sample',c(1:samples),sep=''))], 1, function(x) { length(x[which(abs(x) >= abs(x[1]))]) / length(x) }) # note this function includes an inherent +1 because it includes the RealDifference itself
    bootstrapped_splines$Diverged <- ifelse(bootstrapped_splines$p_value < alpha, 1, 0)
  }
  else {
    # within-subjects:
    
    # for within-subjects, we need to calculate the difference between
    # level 1 and 2 of the factor for each subject before sampling splines
    library(reshape2)
    data <- dcast(data, as.formula(paste(paste(data_options$participant_factor,'t1',sep=" + "),factor,sep=' ~ ')), value.var='BinMean', fun.aggregate = mean, drop = TRUE)
    
    # re-calculate BinMean as the DIFFERENCE between the two labels
    data$BinMean <- data[, 3] - data[, 4]
    
    # remove all samples where BinMean == NA
    data <- data[!is.na(data$BinMean), ]
    
    data[, data_options$participant_factor] <- factor(data[, data_options$participant_factor])
    
    # fit splines for each participant
    splines <- fit_splines(data)
    
    # we now have our spline fit for the real data
    # let's random shuffle the condition labels and see how likely these divergences are
    # by chance
    
    # this dataframe will hold each of the mean differences between conditions calculated
    # at each timepoint after shuffling condition labels (i.e., by random chance)
    bootstrapped_splines <- data.frame(matrix(nrow=length(seq(min(data$t1), max(data$t1), by=resolution)), ncol=(samples + 4)))
    colnames(bootstrapped_splines) <- c('t1','RealDifference','p_value','Diverged',paste('Sample',c(1:samples),sep=''))
    bootstrapped_splines$t1 <- seq(min(data$t1), max(data$t1), by=resolution)
    bootstrapped_splines$RealDifference <- splines$Mean
    
    for (i in 1:samples) {
      # shuffle conditions
      for (participant in as.vector(unique(data$ParticipantName))) {
        if (rbinom(1,1,.5) == 1) {
          # shuffle!
          # all we actually have to do "shuffle" this data is flip the sign on the BinMean for this
          # participant, because it's already cast down to a difference score
          
          data[which(data$ParticipantName == participant), 'BinMean'] <- -data[which(data$ParticipantName == participant), 'BinMean']
        }
        else {
          # no shuffling here...
        }
      }
      
      spline_fits <- fit_splines(data)
      
      # now add to bootstrapped_splines where we keep track of these random differences
      bootstrapped_splines[, paste('Sample',i,sep='')] <- spline_fits$Mean
    }
    
    # now how many times did a divergence this big happen by random chance with shuffled labels?
    bootstrapped_splines$p_value <- apply(bootstrapped_splines[, c('RealDifference',paste('Sample',c(1:samples),sep=''))], 1, function(x) { length(x[which(abs(x) >= abs(x[1]))]) / length(x) }) # note this function includes an inherent +1 because it includes the RealDifference itself
    bootstrapped_splines$Diverged <- ifelse(bootstrapped_splines$p_value < alpha, 1, 0)
  }
  
  # return everything!
  list(
    bootstrapped_splines = bootstrapped_splines,
    splines = splines
  )
}

# get_looks(data, data_options, smoothing, factors)
#
# Find all looks to AOIs in a dataset and describe them. This function fills in 1-sample gaps
# caused by trackloss between sequential gazes to the same location.
#
# @param dataframe data
# @param list data_options
# @param boolean smoothing
# @param character.vector factors
#
# @return dataframe looks
get_looks <- function (data, data_options, smoothing = 1, factors = NA) {
  # set defaults if necessary
  if (length(factors) < 2 && is.na(factors)) {
    factors <- data_options$default_factors
  }
  
  # make sure all outside looks are treated as trackloss
  data <- treat_outside_looks_as_trackloss(data, data_options)
  
  # first, make sure all trackloss results in an NA within the AOI column
  data[which(data[, data_options$trackloss_factor] == 1), data_options$active_aoi_factor] <- NA
             
  # for each AOI within each trial within each subject,
  # do some smoothing to get rid of trackloss issues
  
  looks <- data.frame(matrix(nrow = 0, ncol = 11 + length(factors)))
  
  # we'll store this in column_names so we can keep using it
  column_names <- c(data_options$participant_factor, factors, data_options$trial_factor, data_options$active_aoi_factor, 'SampleLength', 'StartSample', 'EndSample', 'TimeLength', 'StartTime', 'EndTime', 'RankLength', 'RankSequence')
  
  colnames(looks) <- column_names
  
  for (participant in unique(data[, data_options$participant_factor])) {
    for (factor in factors) {
      for (factor_level in unique(data[, factor])) {
        for (trial in unique(data[, data_options$trial_factor])) {
          # get matching rows for this operation
          trial_rows <- which(data[, data_options$participant_factor] == participant & data[, factor] == factor_level & data[, data_options$trial_factor] == trial);
          
          # calculate AOI runs within these trial rows
          # then look for any chances for smoothing
          runs <- rle(as.character(data[trial_rows, data_options$active_aoi_factor]))
          
          for (i in 1:length(runs$values)) {
            if (length(runs$values[i]) > 1 && is.na(runs$values[i]) && i > 1) {
              # if the NA look is <= our smoothing sample value, let's fill it in with
              # the last value
              if (runs$lengths[i] <= smoothing && !is.na(runs$values[i - 1])) {
                # smooth it!
                # kill this value by setting its length to 0
                runs$lengths[i] <- 0
                
                # then add a length to the preceding value
                runs$lengths[i - 1] <- runs$length[i - 1] + 1
              }
            }
          }
          
          # regenerate runs with the smoothed dataset
          runs <- rle(inverse.rle(runs))
          
          if (length(runs$values) > 0) {          
            # now add the final runs to the looks dataframe
            looks_to_bind <- data.frame(matrix(nrow = length(runs$values), ncol = 11 + length(factors)))
            colnames(looks_to_bind) <- column_names
            
            looks_to_bind[, data_options$participant_factor] <- participant
            
            for (factor in factors) {
              looks_to_bind[, factor] <- as.character(data[trial_rows[1], factor])
            }
            
            looks_to_bind[, data_options$trial_factor] <- trial
            looks_to_bind[, data_options$active_aoi_factor] <- runs$values
            looks_to_bind$SampleLength <- runs$lengths
            looks_to_bind$StartSample <- c(1,cumsum(runs$lengths)[1:length(runs$lengths) - 1] + 1)
            looks_to_bind$EndSample <- cumsum(runs$lengths) + 1
            looks_to_bind$TimeLength <- looks_to_bind$SampleLength * (1000/data_options$sample_rate)
            looks_to_bind$StartTime <- (looks_to_bind$StartSample - 1) * (1000/data_options$sample_rate)
            looks_to_bind$EndTime <- (looks_to_bind$EndSample - 1) * (1000/data_options$sample_rate)
            looks_to_bind$RankLength <- rank(-looks_to_bind$SampleLength)
            looks_to_bind$RankSequence <- 1:length(runs$lengths)
            
            # bind!
            looks <- rbind(looks, looks_to_bind)
          }
        }
      }
    }
  }
  
  looks
}








se <- function(x) {
  y <- x[!is.na(x)] # remove the missing values, if any
  sqrt(var(as.vector(y))/length(y))
}