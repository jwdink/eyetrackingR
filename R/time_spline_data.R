#' Bootstrap resample splines for time-series data.
#'
#' Deprecated. Performing this analysis should be done by calling \code{analyze_time_bins(test="boot_splines")}. 
#' 
#' This method builds confidence intervals around proportion-looking data by bootstrap resampling.
#' Data can be smoothed by fitting smoothing splines. This function performs the bootstrap resampling,
#' \code{analyze_boot_splines} generates confidence intervals and tests for divergences.
#' 
#' Limited to statistical test between two conditions.
#' 
#' @export
make_boot_splines_data = function(data, predictor_column, within_subj, aoi, bs_samples, smoother, resolution, alpha, ...) {
  UseMethod("make_boot_splines_data")
}
#' @describeIn make_boot_splines_data
#'
#' @param data The output of \code{time_sequence_data()}
#' @param predictor_column What predictor var to split by? Maximum two conditions
#' @param within_subj Are the two conditions within or between subjects?
#' @param aoi Which AOI do you wish to perform the analysis on?
#' @param bs_samples How many iterations to run bootstrap resampling? Default 1000
#' @param smoother Smooth data using "smooth.spline," "loess," or "none" for no smoothing
#' @param resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100
#'   intervals per second, or hundredths of a second. Default is the same size as time-bins.
#' @param alpha p-value when the groups are sufficiently "diverged"
#' @param ... Ignored
#' 
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE )
#' response_window <- subset_by_window(data, window_start_time = 15500, 
#'                                     window_end_time = 21000, rezero = FALSE)
#' response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
#'                                          predictor_columns = "Sex", 
#'                                          summarize_by = "ParticipantName")
#'                                          
#' df_bootstrapped <- make_boot_splines_data(response_time, 
#'                                           predictor_column = 'Sex', 
#'                                           within_subj = FALSE, 
#'                                           bs_samples = 500, 
#'                                           alpha = .05,
#'                                           smoother = "smooth.spline") 
#' 
#' 
#' @export
#' @return A bootstrapped distribution of samples for each time-bin
make_boot_splines_data.time_sequence_data <- function (data,
                                                       predictor_column,
                                                       within_subj,
                                                       aoi = NULL,
                                                       bs_samples = 1000,
                                                       smoother = "smooth.spline",
                                                       resolution = NULL,
                                                       alpha = .05, ...) {
  
  # get attrs:
  attrs <- attr(data, "eyetrackingR")
  data_options <- attrs$data_options
  called_from_top <- !isNamespace(topenv(parent.frame(1)))
  if (called_from_top) {
    .Deprecated(msg = "Calling boot-splines from this function is deprecated. Please use `analyze_time_bins` for boot-splines.")
  }
  dots <- lazyeval::lazy_dots(...)
  if ( !is.null(dots$samples) ) {
    if (missing(bs_samples)) {
      bs_samples <- eval(dots$samples$expr)
      warning("The 'samples' argument is deprecated, please use 'bs_samples' in the future.", call. = FALSE)
    }
    for (extra_arg in names(dots)) {
      if (extra_arg!="samples") warning("Argument '", extra_arg, "' ignored.", call. = FALSE)
    }
  }
  
  
  
  # check predictor:
  if (!is.factor(data[[predictor_column]])) {
    message("Coercing ", predictor_column, " to factor...")
    data[[predictor_column]] <- factor(data[[predictor_column]])
  }

  # Make sure data is summarized:  
  summarized_by <- attrs$summarized_by
  if (is.null(summarized_by)) stop("This analysis requires summarized data. ",
                                   "When using the 'make_time_sequence_data' function, please select an argument for 'summarize_by'",
                                   " (e.g., the participant column).")
  if (length(summarized_by)>1) stop("Data should only be summarized by one thing (e.g., participant, item, not both). ",
                                    "Participant/Item analyses should be run separately.")
  
  # validate arguments
  if ( length(levels(as.factor(data[[predictor_column]]))) != 2 ) {
    stop('make_boot_splines_data requires a predictor_column with exactly 2 levels.')
  }
  smoother <- match.arg(smoother, c('smooth.spline','loess','none'))
  if (is.null(resolution)) resolution = attrs$time_bin_size

  # Pre-prep data
  if (is.null(aoi)) {
    if (length(unique(data$AOI)) > 1) stop("Please specify which AOI you wish to bootstrap.")
  } else {
    data <- filter(data, AOI == aoi)
  }
  data <- data[ !is.na(data[[predictor_column]]), ]

  # define sampler/bootstrapper:
  sampler <- function (run_original, run_subjects_rows, data_options, resolution, smoother) {

    # Take a list where each element corresponds to the rows of the subject.
    # Sample w/o replacement N elements, and concatenate their contents
    # This gives you a vector specifying which rows to extract, in order to extract the data corresponding to the sampled subjects
    sampled_subject_rows <- unlist(sample(run_subjects_rows, length(run_subjects_rows), replace = TRUE))
    run_data <- run_original[sampled_subject_rows,]
    
    if (smoother == "none") {
      # use straight linear approximation on the values
      run_predicted_times <- seq(min(run_original$Time), max(run_original$Time), by=resolution)

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
      run_predicted_times <- seq(min(run_original$Time), max(run_original$Time), by=resolution)
      run_predictions <- predict(run_spline, run_predicted_times)
      return(run_predictions$y)
    }
    else if (smoother == 'loess') {
      # loess!
      run_loess <- with(run_data,
                        loess(Prop ~ Time))

      # get interpolated loess predictions for total time at *resolution*
      run_predicted_times <- seq(min(run_original$Time), max(run_original$Time), by=resolution)
      run_predictions <- predict(run_loess, run_predicted_times)

      return (run_predictions)
    }
    
  }

  # this dataframe will hold our final dataset
  combined_bootstrapped_data <- data.frame()

  if (within_subj == FALSE) {
    # between-subjects:
    for (level in unique(data[[predictor_column]]) ) {
      # subset for condition level
      subsetted_data <- data[which(data[, predictor_column] == level),]
      subsetted_data$RowNum <- 1:nrow(subsetted_data)

      # get the rows for each subject/item
      run_subjects <- unique(subsetted_data[[summarized_by]])
      run_subjects_rows <- lapply(run_subjects, function(sub) subsetted_data$RowNum[ subsetted_data[[summarized_by]] == sub ])

      # bootstrap
      bootstrapped_data <- replicate(bs_samples, sampler(subsetted_data, run_subjects_rows, data_options, resolution, smoother))
      bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))

      # label each sample by number
      sample_rows <- paste('Sample', c(1:bs_samples), sep="")
      colnames(bootstrapped_data) <- sample_rows

      bootstrapped_data <- data.frame(stringsAsFactors = FALSE,
                                      predictor_column = level,
                                      Time = seq(min(subsetted_data$Time), max(subsetted_data$Time), by=resolution),
                                      bootstrapped_data
      )
      colnames(bootstrapped_data)[1] <- predictor_column

      #
      combined_bootstrapped_data <- bind_rows(combined_bootstrapped_data,bootstrapped_data)
      combined_bootstrapped_data[[predictor_column]] <- factor(combined_bootstrapped_data[[predictor_column]], 
                                                               levels = levels(data[[predictor_column]]))
    }
  }
  else {
    # within-subjects:

    # Group by participant, timebin; Calculate the difference in proportion between level1 and level2
    level1 <- levels(data[[predictor_column]])[1]
    level2 <- levels(data[[predictor_column]])[2]
    df_grouped <- group_by_(data, .dots = c(summarized_by, "Time") )
    df_diff <- summarize_(df_grouped,
                         .dots = list(Prop1 = interp(~mean(Prop[PRED_COL == level1]), PRED_COL = as.name(predictor_column)),
                                      Prop2 = interp(~mean(Prop[PRED_COL == level2]), PRED_COL = as.name(predictor_column)),
                                      Prop  = interp(~Prop1 - Prop2)
                         ))

    # remove all samples where Prop == NA;
    df_diff <- df_diff[!is.na(df_diff$Prop), ]

    df_diff$RowNum <- 1:nrow(df_diff)

    # get rows for each subject/item
    run_subjects <- unique(df_diff[[summarized_by]])
    run_subjects_rows = lapply(run_subjects, function(sub) df_diff$RowNum[ df_diff[[summarized_by]] == sub ])

    # bootstrap
    bootstrapped_data <- replicate(bs_samples, sampler(df_diff, run_subjects_rows, data_options, resolution, smoother))
    bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))

    sample_rows <- paste('Sample', c(1:bs_samples), sep="")
    colnames(bootstrapped_data) <- sample_rows

    bootstrapped_data <- data.frame(
      Time = seq(min(df_diff$Time), max(df_diff$Time), by=resolution),
      bootstrapped_data
    )

    combined_bootstrapped_data <- bootstrapped_data
  }

  # Assign class information:
  combined_bootstrapped_data <- as.data.frame(combined_bootstrapped_data)
  class(combined_bootstrapped_data) <- c('boot_splines_data', class(combined_bootstrapped_data))
  attr(combined_bootstrapped_data, 'eyetrackingR') = list(
    bootstrapped = list(data_options = data_options,
                        within_subj = within_subj,
                        predictor_column = predictor_column,
                        samples = bs_samples,
                        alpha = alpha,
                        resolution = resolution,
                        min_time = min(combined_bootstrapped_data[['Time']])
    ))

  return(combined_bootstrapped_data)
}

#' Estimate confidence intervals for bootstrapped splines data
#' 
#' Deprecated. Performing this analysis should be done by calling \code{analyze_time_bins(test="boot_splines")}. 
#'
#' Estimates a confidence interval over the difference between means (within- or between-subjects)
#' from \code{boot_splines_data}. Confidence intervals are derived from the alpha argument in 
#' \code{boot_splines_data} (e.g., alpha = .05, CI=(.025,.975); alpha=.01, CI=(.005,.0995))
#' @export
analyze_boot_splines <- function(data) {
  UseMethod("analyze_boot_splines")
}
#' @describeIn analyze_boot_splines
#'
#' @param  data The output of the \code{boot_splines_data} function
#' 
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE )
#' response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
#'                                     rezero = FALSE)
#' response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
#'                                          predictor_columns = "Sex", 
#'                                          summarize_by = "ParticipantName")
#'                                          
#' # bootstrap resample 500 smoothed splines from the dataset,
#' # comparing females versus females at an alpha of .05                                         
#' df_bootstrapped <- make_boot_splines_data(response_time,
#'                                           predictor_column = 'Sex',
#'                                           within_subj = FALSE,
#'                                           bs_samples = 500,
#'                                           alpha = .05,
#'                                           smoother = "smooth.spline")
#' 
#' # analyze the divergences that occurred
#' boot_splines_analysis <- analyze_boot_splines(df_bootstrapped)
#' summary(boot_splines_analysis)
#' 
#' @export
#' @return A dataframe indicating means and CIs for each time-bin
analyze_boot_splines.boot_splines_data <- function(data) {
  
  ## Helpers:
  .get_nonparametric_stat <- function(x,y) {
    .weighted_iqr <- function(x,y) {
      iqr_x <- diff(quantile(x, probs = c(.05, .95)))
      iqr_y <- diff(quantile(y, probs = c(.05, .95)))
      (iqr_x*length(x) + iqr_y*length(y))/length(c(x,y))
    }
    (median(x)-median(y)) / .weighted_iqr(x,y)
  }

  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(data, "eyetrackingR")
  bootstrap_attr = attrs$bootstrapped
  data_options = attrs$data_options
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later

  called_from_top <- !isNamespace(topenv(parent.frame(1)))
  if (called_from_top) {
    .Deprecated(msg = "Calling boot-splines from this function is deprecated. Please use `analyze_time_bins` for boot-splines.")
  }
  
  
  # adjust CI based on alpha
  low_prob <- .5 - ((1-bootstrap_attr$alpha)/2)
  high_prob <- .5 + ((1-bootstrap_attr$alpha)/2)

  if (bootstrap_attr$within_subj == TRUE) {
    
    data_gathered <- tidyr::gather_(data,key_col = "Sample", value_col = "Val", 
                                      gather_cols = paste0("Sample", 1:bootstrap_attr$samples ) )
    data_summarized <- summarize(.data = group_by(Time, .data = data_gathered),
                                   MeanDiff = mean(Val, na.rm=TRUE),
                                   SE = sd(Val, na.rm=TRUE),
                                   CI_low  = quantile(Val, probs = low_prob, na.rm=TRUE),
                                   CI_high = quantile(Val, probs = high_prob, na.rm=TRUE),
                                   Statistic = .get_nonparametric_stat(Val, 0))
    bootstrapped_data <- mutate(ungroup(data_summarized),
                                Significant = (CI_high > 0 & CI_low > 0) | (CI_high < 0 & CI_low < 0) )
  }
  else {
    
    data_gathered <- tidyr::gather_(data, key_col = "Sample", value_col = "Val", 
                                      gather_cols = paste0("Sample", 1:bootstrap_attr$samples ))
    data_spread <- tidyr::spread_(data_gathered, key_col = bootstrap_attr$predictor_column, value_col = "Val")
    colnames(data_spread)[3:4] <- c('Lvl1','Lvl2')
    data_spread$Val <- with(data_spread, Lvl1-Lvl2)
    
    data_summarized <- summarize(.data = group_by(Time, .data = data_spread),
                                 MeanDiff = mean(Val, na.rm=TRUE),
                                 SE = sd(Val, na.rm=TRUE),
                                 CI_low  = quantile(Val, probs = low_prob, na.rm=TRUE),
                                 CI_high = quantile(Val, probs = high_prob, na.rm=TRUE),
                                 Statistic = .get_nonparametric_stat(Lvl1, Lvl2))
    bootstrapped_data <- mutate(ungroup(data_summarized),
                                Significant = (CI_high > 0 & CI_low > 0) | (CI_high < 0 & CI_low < 0) )
  }

  bootstrapped_data = as.data.frame(bootstrapped_data)
  class(bootstrapped_data) = c('boot_splines_analysis', class(bootstrapped_data))
  attr(bootstrapped_data, 'eyetrackingR') = list(bootstrapped = bootstrap_attr,
                                                 data_options = data_options)

  return(bootstrapped_data)
}

#' Summary Method for Bootstrapped Splines Analysis
#' @param  object The output of the \code{boot_splines_data} function
#' @param ... Ignored
#' @export
#' @return Prints a list of divergence-times.
summary.boot_splines_analysis <- function(object, ...) {

  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(object, "eyetrackingR")
  data_options = attrs$data_options
  bootstrap_attr = attrs$bootstrapped
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later

  # find divergences as runs of Significant == TRUE
  divergences <- rle(c(FALSE,object$Significant)) # TO DO: this should probably list times start_first_bin -> end_last_bin,
                                                # instead of current behavior of start_first_bin -> start_last_bin

  if (sum(divergences$values) == 0) {
    cat("No Divergences.")
  }
  else {
    # convert to time ranges
    divergences$lengths <- (divergences$lengths * bootstrap_attr$resolution)
    divergences$timestamps <- cumsum(divergences$lengths) + bootstrap_attr$min_time-bootstrap_attr$resolution

    divergences <- paste0(seq_along(divergences$timestamps[which(divergences$values == TRUE)]), ":\t",
                          divergences$timestamps[which(divergences$values == TRUE)-1],
                          ' - ',
                          divergences$timestamps[which(divergences$values == TRUE)])

    cat("Divergences:\n")
    cat(paste(divergences, collapse="\n"))
  }
}

#' Plot bootstrapped-splines data
#'
#' Plot the means and CIs of bootstrapped splines (either within-subjects or between-subjects)
#'
#' @param  x The output of the \code{make_boot_splines_data} function
#' @param ... Ignored
#' @export
#' @return A ggplot object
plot.boot_splines_data = function(x, ...) {
  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(x, "eyetrackingR")
  data_options = attrs$data_options
  bootstrap_attr = attrs$bootstrapped
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later

  # if within-subjects, plot difference score
  if (bootstrap_attr$within_subj == TRUE) {
    # use plot.boot_splines_analysis() to plot within-subjects difference
    # because, for a within-subjects test, this is all that matters
    message("Plotting within-subjects differences...")
    data <- analyze_boot_splines(x)

    return (plot(data))
  }
  else {
    x$Mean <- apply(x[, paste0('Sample',1:bootstrap_attr$samples)], 1, mean)
    x$SE <- apply(x[, paste0('Sample',1:bootstrap_attr$samples)], 1, sd)

    low_prob <- .5 - ((1-bootstrap_attr$alpha)/2)
    high_prob <- .5 + ((1-bootstrap_attr$alpha)/2)

    x$CI_high <- round(apply(x[, paste0('Sample',1:bootstrap_attr$samples)], 1, function (x) { quantile(x,probs=high_prob, na.rm=TRUE) }),5)
    x$CI_low <- round(apply(x[, paste0('Sample',1:bootstrap_attr$samples)], 1, function (x) { quantile(x,probs=low_prob, na.rm=TRUE) }),5)

    g <- ggplot(x, aes_string(x='Time', y='Mean', color=bootstrap_attr$predictor_column)) +
      geom_line() +
      geom_ribbon(aes_string(ymax='CI_high', ymin='CI_low', fill=bootstrap_attr$predictor_column), alpha=.2, colour=NA) +
      xlab('Time') +
      ylab('Proportion')
  }

  g
}

#' Plot differences in bootstrapped-splines data
#'
#' Plot the means and CIs of bootstrapped spline difference estimates and intervals
#' (either within-subjects or between-subjects)
#'
#' @param x The output of the \code{analyze_boot_splines} function
#' @param ... Ignored
#' @export
#' @return A ggplot object
plot.boot_splines_analysis <- function(x, ...) {

  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(x, "eyetrackingR")
  data_options = attrs$data_options
  bootstrap_attr = attrs$bootstrapped
  if (is.null(bootstrap_attr)) stop("Dataframe has been corrupted.") # <----- fix later

  # we have a MeanDiff and CI for both within- and between-subjects...
  g <- ggplot(x, aes(x=Time, y=MeanDiff)) +
    geom_line() +
    geom_ribbon(aes(ymax=CI_high, ymin=CI_low), alpha=.2, colour=NA) +
    xlab('Time') +
    geom_hline(yintercept = 0, linetype="dashed")

  if (bootstrap_attr$within_subj == TRUE) {
    g <- g + ylab('Difference Score (within-subjects)')
  }
  else {
    g <- g + ylab('Difference Score (between-subjects)')
  }

  g
}
