#' make_time_sequence_data()
#'
#' Creates time-bins and summarizes proportion-looking within each time-bin
#'
#' @param data
#' @param data_options
#' @param time_bin_size
#' @param aois              Which AOIs are of interest? Defaults to all in 'data_options'
#' @param predictor_columns Which columns indicate predictor vars, and therefore should be preserved in
#'   grouping operations?
#' @param summarize_by      Should the data be summarized along, e.g., participants, items, etc.? If so, give
#'   column name(s) here. If left blank, will leave trials distinct. The former is needed for more traditional
#'   analyses (t.tests, ANOVAs), while the latter is preferable for mixed-effects models (lmer)
#'
#' @return Data binned into time-bins, with proportion-looking and transformations as well as orthogonal
#'   time-polynomials for growth curve analysis

make_time_sequence_data <- function (data,
                        data_options,
                        time_bin_size,
                        aois = NULL,
                        predictor_columns = NULL,
                        summarize_by = NULL) {


  require("dplyr", quietly=TRUE)
  require("lazyeval", quietly = TRUE)

  if (is.null(aois)) aois = data_options$aoi_columns

  # For Multiple AOIs:
  if (length(aois) > 1) {
    list_of_dfs = lapply(X = aois, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      make_time_sequence_data(data, data_options, time_bin_size, this_aoi, predictor_columns, summarize_by)
    })
    out <- bind_rows(list_of_dfs)
    attrs <- attr(out,"eyetrackingR")
    new_attrs <- list(summarized_by = summarize_by)
    attr(out,"eyetrackingR") = as.list(c(attrs, new_attrs))
    out <- as.data.frame(out)
    class(out) <- c('time_sequence_data', class(out))
    return( out )
  }

  # Prelims:
  data <- ungroup(data)
  aoi_col <- as.name(aois)

  # Make Time Bins:
  data[["TimeBin"]] <- floor(data[[data_options$time_column]] / time_bin_size)
  # check that the last time bin has an appropriate amount of data inside it
  # (e.g., it's not just a couple of samples at the end of the trial)
  df_grouped <- group_by_(data, .dots = c("TimeBin", data_options$trial_column))
  df_tb_sum <- summarise_(df_grouped, .dots = list(N = interp(~n_distinct(TIME), TIME = as.name(data_options$time_column))) )
  df_tb_sum <- summarise(df_tb_sum, N = mean(N))
  too_small_tb <- df_tb_sum$N[which.max(df_tb_sum$TimeBin)] < median(df_tb_sum$N, na.rm=TRUE) / 3
  if (too_small_tb) warning("With the current time-bin size, the final time-bin is very small. ",
                            "Consider choosing a different time-bin size or using subset_by_window ",
                            "to remove this portion of the trial.")

  # Make Summary
  if (is.null(summarize_by)) {
    groups <- c(data_options$participant_column,
               data_options$item_columns,
               data_options$trial_column,
               predictor_columns, "TimeBin")
  } else {
    groups <- c(summarize_by, predictor_columns, "TimeBin")
  }
  df_summarized <- .make_proportion_looking_summary(data=data, groups = groups, aoi_col)
  df_summarized[["Time"]] <- df_summarized[["TimeBin"]] * time_bin_size

  # Add orthogonal polynomials for Growth Curve Analyses
  time_bin_column <- df_summarized$TimeBin
  max_degree <- min( length(unique(time_bin_column))-1 , 7 )
  if (max_degree < 7) warning("Fewer time bins than polynomial degrees-- consider decreasing size of time bin.")
  orthogonal_polynomials <- poly(sort(as.vector(unique(time_bin_column))), max_degree)
  time_codes <- data.frame(
    sort(as.vector(unique(time_bin_column))),
    orthogonal_polynomials[, c(1:max_degree)]
  )
  colnames(time_codes) <- c('TimeBin',paste0("ot", 1:max_degree))

  out <- left_join(df_summarized, time_codes, by='TimeBin')

  out <- as.data.frame(out)
  class(out) <- c('time_sequence_data', class(out))
  attrs <- attr(out,"eyetrackingR")
  new_attrs <- list(
    data_options = data_options,
    summarized_by = summarize_by,
    time_bin_size = time_bin_size)
  attr(out,"eyetrackingR") <- as.list(c(attrs, new_attrs))
  return(out)

}

#' analyze_time_bins()
#'
#' Runs a test on each time-bin of a time-analysis. Supports \code{t.test}, \code{wilcox.test}, \code{lm}, and
#' \code{lmer}
#'
#' @param data   The output of the 'make_time_sequence_data' function
#' @param data_options
#' @param predictor_column  The variable whose test statistic you are interested in
#' @param test              What type of test should be performed in each time bin? Supports t.test, wilcox,
#'   lm, or lmer.
#' @param threshold         Value of statistic used in determining significance
#' @param alpha             Alpha value for determining significance, ignored if threshold is given
#' @param formula           What formula should be used for the test? Optional for all but lmer, if unset will
#'   use \code{Prop ~ [predictor_column]}
#' @param return_model      In the returned dataframe, should a model be given for each time bin, or just the
#'   summary of those models?
#' @param ...               Any other arguments to be passed to the selected 'test' function (e.g., paired,
#'   var.equal, etc.)
#' @return A dataframe indicating the results of the test at each time-bin.
analyze_time_bins = function(data, ...) {
  UseMethod("analyze_time_bins")
}
analyze_time_bins.time_sequence_data <- function(data,
                              data_options,
                              predictor_column,
                              test,
                              threshold = NULL,
                              alpha = .05,
                              formula = NULL,
                              return_model = FALSE,
                              quiet = FALSE,
                              ...)
{

  ## Helper:
  .fix_unpaired = function(data, data_options, predictor_column) {
    lvl1 <- levels(data[[predictor_column]])[1]
    df_grouped <- group_by_(data, .dots = c(data_options$participant_column))
    df_mutated <- mutate_(df_grouped,
                          .dots = list(PairedObs = interp(~length(which(COND_COL == lvl1)) > 0 & length(which(COND_COL != lvl1)) > 0,
                                                          COND_COL = as.name(predictor_column)))
    )
    df_filtered <- filter(df_mutated, PairedObs)
    ungroup(df_filtered)
  }

  # Prelims:
  if (!require("lme4")) {
    if (test == "lmer") stop("Please install the 'lme4' package to use this method.")
  }
  test <- match.arg(test, c("t.test","wilcox.test","lm","lmer"))

  # For Multiple aois:
  if (!'AOI' %in% colnames(data)) stop("'AOI' column is missing from data.")
  aois <- unique(data[['AOI']])
  if ( length(aois) > 1 ) {
    list_of_dfs <- lapply(X = aois, FUN = function(this_aoi) {
      if (!quiet) message("Analyzing ", this_aoi, "...")
      this_df <- filter(data, AOI == this_aoi)
      class(this_df) = class(data)
      analyze_time_bins(data = this_df, data_options, predictor_column, test, threshold, alpha, formula, return_model, quiet, ...)
    })
    out <- bind_rows(list_of_dfs)
    out <- as.data.frame(out)
    class(out) = c('bin_analysis', class(out))
    return( out )
  }

  # Check that data is collapsed (by e.g. participants):
  if (test != "lmer") {
    attrs <- attr(data, "eyetrackingR")
    summarized_by <- attrs$summarized_by
    if (is.null(summarized_by)) stop(test, " requires summarized data. ",
                                     "When using the 'make_time_sequence_data' function, please select an argument for 'summarize_by'",
                                     " (e.g., the participant column).")
  }

  # auto-make a formula, unless they specified one
  if (is.null(formula)) {
    if (test=="lmer") stop("Must specify a formula if using lmer.")
    formula <- as.formula(paste("Prop ~", predictor_column))
  }

  # Run a model for each time-bin
  paired <- list(...)[["paired"]]
  if (!quiet) message("Computing ", test, " for each time bin...")
  failsafe_test <- failwith(default = NA, f = get(test), quiet = quiet)
  if (quiet) pblapply <- lapply
  models= pblapply(unique(data$Time), function(tb) {
    # get data:
    temp_dat <- filter(data, Time==tb)
    # Make paired test more robust to unpaired observations within a bin:
    if (identical(paired, TRUE)) temp_dat <- .fix_unpaired(temp_dat, data_options, predictor_column)
    # make model:
    model <- failsafe_test(formula = formula, data = temp_dat, ... = ...)
    # get N:
    if (test=="wilcox.test" | test=="lm") {
      predictor_col_is_na <- is.na(temp_dat[[predictor_column]])
      model$sample_size <- length(unique( temp_dat[[data_options$participant_column]][!predictor_col_is_na] ))
    }
    model
  })

  # Get Statistic:
  if (test=="lmer") {
    tidied_models <- lapply(models, broom::tidy, effects="fixed")
  } else {
    tidied_models <- lapply(models, broom::tidy)
  }
  if (test %in% c('t.test','wilcox.test')) {
    models_statistics <- sapply(tidied_models, function(x) ifelse('statistic' %in% names(x), x[,'statistic'], NA) )
  } else {
    models_statistics <- sapply(tidied_models, function(x) {
      which_row <- grep(pattern = predictor_column, x = x[['term']], fixed = TRUE) # look for partially matching param (for treatment coding)
      if (length(which_row)==1) {
        return(x[which_row, 'statistic'])
      } else {
        # too many matches? look for exact match (happens with continous predictor)
        which_row <- which(x[['term']] == predictor_column)
        if (length(which_row)==1) return(x[which_row, 'statistic'])
        warning("Could not find the parameter '",predictor_column,"' in your model. Found instead: ", paste(x[['term']], collapse=", ") )
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
      dfs <- sapply(tidied_models, function(x) ifelse('parameter' %in% names(x), x[,'parameter'], NA))
      crit_pos <- qt(1-alpha/2, df = dfs)
      crit_neg <- -crit_pos
    } else if (test=="wilcox.test") {
      sample_sizes <- sapply(models, function(x) x$sample_size)
      crit_pos <- qsignrank(p = 1-alpha/2, n = sample_sizes )
      crit_neg <- -crit_pos
    } else if (test=="lm") {
      sample_sizes <- sapply(models, function(x) x$sample_size)
      crit_pos <- qt(1-alpha/2, df = sample_sizes-1)
      crit_neg <- -crit_pos
    }
  } else {
    crit_pos <- ifelse(sign(threshold)==1,  threshold, -threshold)
    crit_neg <- ifelse(sign(threshold)==1, -threshold,  threshold)
  }

  # Return DataFrame:
  out <- data.frame(stringsAsFactors = FALSE,
                   Statistic = models_statistics,
                   CritStatisticPos = crit_pos,
                   CritStatisticNeg = crit_neg,
                   Time = unique(data$Time)) # same order as for loop that built models
  out$AOI <- data$AOI[1]
  if (return_model) out$Model <- models

  out <- as.data.frame(out)
  class(out) <- c('bin_analysis', class(out))
  attr(out,"eyetrackingR") <- list(formula= formula)
  out
}

#' Plot time-sequence data
#'
#' Plot the timecourse of looking. Each AOI will be plotted in a separate pane, and data can be split into
#' groups by a predictor column (median split if numeric).
#'
#' @param data
#' @param predictor_column
#'
#' @return A ggplot object
plot.time_sequence_data <- function(data, predictor_column=NULL, dv='Prop') {

  data_options = attr(data, "eyetrackingR")$data_options

  ## Collapse by-subject for plotting
  df_plot <- group_by_(data, .dots = c(data_options$participant_column, "Time", "AOI", predictor_column))
  summarize_arg <- list(interp(~mean(DV, na.rm=TRUE), DV = as.name(dv)))
  names(summarize_arg) <- dv
  df_plot <- summarize_(df_plot, .dots = summarize_arg)

  ## Check condition predictor
  if ( length(predictor_column) > 1 ) {
    stop('Can only plot time-analysis for one predictor at a time.')
  }
  numeric_predictor_col = FALSE
  if (!is.null(predictor_column)) {
    if (is.numeric(df_plot[[predictor_column]])) numeric_predictor_col <- TRUE
  }

  ## Plot:
  if (numeric_predictor_col) {
    message("Condition factor is numeric, performing median split...")
    the_median <- median(df_plot[[predictor_column]], na.rm=TRUE)
    df_plot[["GroupFactor"]] <- ifelse(df_plot[[predictor_column]] > the_median,
                                      paste0("High (>", round(the_median,2), ")"),
                                      "Low")

    g <- ggplot(df_plot, aes_string(x = "Time", y=dv, group="GroupFactor", color="GroupFactor")) +
      stat_summary(fun.y='mean', geom='line') +
      stat_summary(fun.dat='mean_cl_normal', geom='ribbon', mult=1, alpha=.2, colour=NA) +
      facet_wrap( ~ AOI) +
      guides(color= guide_legend(title= predictor_column)) +
      xlab('Time in Trial')
    return(g)

  } else {
    g <- ggplot(df_plot, aes_string(x = "Time", y=dv, group=predictor_column, color=predictor_column, fill=predictor_column)) +
      stat_summary(fun.y='mean', geom='line') +
      stat_summary(fun.data='mean_cl_normal', geom='ribbon', mult=1, alpha=.2, colour=NA) +
      facet_wrap( ~ AOI) +
      xlab('Time in Trial')
    return(g)
  }

}

#' Plot test-statistic for each time-bin in a time-series
#'
#' Plot the result from the \code{analyze_time_bins} function, with the statistic and threshold for each bin
#'
#' @param data
#'
#' @return A ggplot object
plot.bin_analysis <- function(data) {

  ggplot(data = data) +
    geom_line(mapping = aes(x = Time, y= Statistic)) +
    geom_line(mapping = aes(x = Time, y= CritStatisticPos), linetype="dashed") +
    geom_line(mapping = aes(x = Time, y= CritStatisticNeg), linetype="dashed") +
    ylab("Statistic") +
    xlab("Time") +
    facet_wrap( ~ AOI)
}
