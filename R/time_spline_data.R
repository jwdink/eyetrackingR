#' Bootstrap resample splines for time-series data.
#'
#' Bootstrap splines from \code{time_sequence_data()}. This creates a distribution from which a non-parametric
#' analysis can be performed.
#' @export
make_boot_splines_data = function(data, predictor_column, within_subj, aoi,smoother, samples, resolution, alpha) {
  UseMethod("make_boot_splines_data")
}
#' @describeIn make_boot_splines_data
#'
#' @param data The output of \code{time_sequence_data()}
#' @param predictor_column What predictor var to split by? Maximum two conditions
#' @param within_subj Are the two conditions within or between subjects?
#' @param aoi Which AOI do you wish to perform the analysis on?
#' @param smoother Smooth data using "smooth.spline," "loess," or leave NULL for no smoothing
#' @param samples How many iterations to run bootstrap resampling? Default 1000
#' @param resolution What resolution should we return predicted splines at, in ms? e.g., 10ms = 100
#'   intervals per second, or hundredths of a second. Default is the same size as time-bins.
#' @param alpha p-value when the groups are sufficiently "diverged"
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
#'                                           samples = 500, 
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
                                                       smoother = "smooth.spline",
                                                       samples = 1000,
                                                       resolution = NULL,
                                                       alpha = .05) {
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    pbreplicate <- function(n, expr, simplify) replicate(n, expr, simplify)
    message("Install package 'pbapply' for a progress bar in this function.")
  } else {
    pbreplicate <- pbapply::pbreplicate
  }
  
  # get attrs:
  attrs <- attr(data, "eyetrackingR")
  data_options <- attrs$data_options

  # Make sure data is summarized:  
  summarized_by <- attrs$summarized_by
  if (is.null(summarized_by)) stop("This analysis requires summarized data. ",
                                   "When using the 'make_time_sequence_data' function, please select an argument for 'summarize_by'",
                                   " (e.g., the participant column).")

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
      bootstrapped_data <- pbreplicate(samples, sampler(subsetted_data, run_subjects_rows, data_options, resolution, smoother))
      bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))

      # label each sample by number
      sample_rows <- paste('Sample', c(1:samples), sep="")
      colnames(bootstrapped_data) <- sample_rows

      bootstrapped_data <- data.frame(stringsAsFactors = FALSE,
                                      predictor_column = level,
                                      Time = seq(min(subsetted_data$Time), max(subsetted_data$Time), by=resolution),
                                      bootstrapped_data
      )
      colnames(bootstrapped_data)[1] <- predictor_column

      #
      combined_bootstrapped_data <- bind_rows(combined_bootstrapped_data,bootstrapped_data)
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
    bootstrapped_data <- pbreplicate(samples, sampler(df_diff, run_subjects_rows, data_options, resolution, smoother))
    bootstrapped_data <- data.frame(matrix(unlist(bootstrapped_data), nrow=nrow(bootstrapped_data), byrow=FALSE))

    sample_rows <- paste('Sample', c(1:samples), sep="")
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
                        samples = samples,
                        alpha = alpha,
                        resolution = resolution,
                        min_time = min(combined_bootstrapped_data[['Time']])
    ))

  return(combined_bootstrapped_data)
}

#' Estimate confidence intervals for bootstrapped splines data
#'
#' Estimates a confidence interval over the difference between means (within- or between-subjects)
#' from a \code{boot_splines_data} object. Confidence intervals are derived from the alpha
#' used to shape the dataset (e.g., alpha = .05, CI=(.025,.975); alpha=.01, CI=(.005,.0995))
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
#'                                           samples = 500,
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

  # make sure there is the proper kind of data frame, and check its attributes
  attrs = attr(data, "eyetrackingR")
  bootstrap_attr = attrs$bootstrapped
  data_options = attrs$data_options
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
      CI_low = round(apply(samples, 1, function (x) { quantile(x,probs=low_prob, na.rm=TRUE) }),5),
      CI_high = round(apply(samples, 1, function (x) { quantile(x,probs=high_prob, na.rm=TRUE) }),5)
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

    bootstrapped_data <- mutate(bootstrapped_data,
                                Significant = ifelse((CI_high > 0 & CI_low > 0) | (CI_high < 0 & CI_low < 0), TRUE, FALSE))
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
      geom_ribbon(aes_string(ymax='CI_high', ymin='CI_low', fill=bootstrap_attr$predictor_column), mult=1, alpha=.2, colour=NA) +
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
    geom_ribbon(aes(ymax=CI_high, ymin=CI_low), mult=1, alpha=.2, colour=NA) +
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
